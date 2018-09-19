%%%============================================================================
%%% @doc 
%%% A storage mechanism that maps 'cache keys' to the 'process identifiers' of 
%%% the 'cache values' that ares tored against them.
%%%
%%% Mnesia is used to implemented this mapping.
%%% @end
%%%============================================================================

-module(sc_store).


%%%============================================================================
%%% Public Interface
%%%============================================================================


%% The API consists of an init/1 function for initializing the storage system 
%% and three functions that handle the basic CRUD operations (create, read, 
%% update, and delete), where the insert/2 function is used both to create new 
%% entries and to update existing ones.
-export([
  init/0,
  insert/2,
  delete/1,
  lookup/1
]).

-define(TABLE_ID, ?MODULE).


%%%============================================================================
%%% Records
%%%============================================================================

%% NB: A record is a 'named tuple' => {key_to_pid, Key, Pid}
%%
-record(key_to_pid, {key, pid}).


%%%============================================================================
%%% Public Interface Implementation
%%%============================================================================

%% Creates a new Mnesia table named 'key_to_pid'.
init() ->
  % ensure that Mnesia is started and delete any existing data- base schema on 
  % the local node. This incarnation of the cache takes a cavalier attitude 
  % toward data and doesn’t hesitate to overwrite previously used schemas or 
  % tables. This is a cache, after all.
  % Mnesia must not be running. (Calling mnesia:stop() is harmless if Mnesia 
  % hasn’t been started.)
  %
  % After that’s done, init() fetches the list of all simple_cache instances 
  % the resource discovery system has found. (Node names to identify the 
  % instances.) This list of cache nodes contains the cache instance as well, 
  % so that is removed and passed to the function dynamic_db_init/1, that 
  % initialises the mnesia cluster.
  %
  mnesia:stop(),
  mnesia:delete_schema([node()]),
  mnesia:start(),

  io:format("This Node: ~p~n", [node()]),

  % {ok, CacheNodes} = resource_discovery:fetch_resources(simple_cache),
  {ok, CacheNodes} = rd_server:fetch_resources(simple_cache),
  io:format("CacheNodes: ~p~n", [CacheNodes]),

  RemoteNodes = lists:delete(node(), CacheNodes),
  io:format("CacheNodes: ~p~n", [RemoteNodes]),

  dynamic_db_init(lists:delete(node(), CacheNodes)).



insert(Key, Pid) ->
  % Using Mnesia as KV-Store, so, no need for transacitons...
  mnesia:dirty_write(#key_to_pid{key = Key, pid = Pid}).
  % mnesia:transaction(fun -> mnesia:write(#key_to_pid{key=Key, pid=Pid}) end).


%% There is a complication to 'looukp' in a distributed setting, the pid you 
%% get from the lookup could be referring to a dead process. Consider the 
%% following scenario: You have nodes a and b and you insert some data in the 
%% cache on node a. You check that you can look up that data on node b and 
%% get the correct value. You then kill node a and run the same query again on 
%% node b. What happens? The operation fails, because the pid in the Mnesia 
%% database still refers to the storage process which was located on node a, 
%% but is now dead. You need a way to invalidate entries that refer to dead 
%% processes.
%%
%% So... we need to check the process defined by the mnesia key exists before 
%% fetching it. Also, we could clean up the mnesia table when the node is shutting 
%% down (in )
%%
lookup(Key) ->

  io:format("sc_store:lookup~n"),

  % Look up the tuple referenced by the key in the Mnesia table.
  %
  % The table is a set, you can only get zero or one records as result, and a 
  % dirty read is sufficient for your purposes.
  % 
  io:format("mnesia:dirty_read...~n"),
  Read = mnesia:dirty_read(key_to_pid, Key),
  io:format("Mnesia Read: ~p~n", [Read]),

  case mnesia:dirty_read(key_to_pid, Key) of
    [{key_to_pid, Key, Pid}] ->
      % Check the Pid in the Mnesia database still refers to a process is 
      % alive (on this or reote node).
      case is_pid_alive(Pid) of
        true -> 
          {ok, Pid};
        false ->
          io:format("sc_store:lookup - Pid dead ~p.~n", [Pid]),
          {error, not_found}
      end;
    [] -> 
      io:format("sc_store:lookup - []~n"),
      {error, not_found}
  end.


delete(Pid) ->
  % Use a special index based read...
  % For index_read/3:
  % - the first argument is the table name, 
  % - the second is the key on which you want to index (the pid), 
  % - the third indicates which index you want to search (because a table can have several).
  %
  case mnesia:dirty_index_read(key_to_pid, Pid, #key_to_pid.pid) of
    % Capture the {key, pid} if it exists.
    [#key_to_pid{} = Record] ->
      % Delete the record. Return ok.
      mnesia:dirty_delete_object(Record);
    % Keep the methods idempotent.
    _ -> 
      ok
  end.



%%%============================================================================
%%% Private Functions
%%%============================================================================

%% ----------------------------------------------------------------------------
%% @doc
%% This function initializes the database differently depending on whether it 
%% finds some other cache instances in the cluster.
%% @end
%%
%% NB: There’s an important caveat here, and that is that the initial node must 
%% be started alone. If two simple_cache nodes are started simultaneously from 
%% scratch, there’ll be a race condition where both may think that the other 
%% node was the first. As a consequence, no initial schema will ever be created. 
%%
%% This could also be avoided with some additional synchronization in the code.
%%
dynamic_db_init([]) ->
  % Handles the case when you seem to be alone; you create the table just as 
  % Because mnesia:create_schema/1 is not invoked after ensuring that any 
  % previous schema is deleted, the new database schema is implicitly created 
  % and is kept in RAM only. 
  % 
  % At this point, you have a working simple_cache instance that is ready to 
  % replicate its data to any other instances that join the cluster.
  %
  io:format("Creating first node mnesia tables!"),
  mnesia:create_table(
    key_to_pid,
    [{index, [pid]},
    {attributes, record_info(fields, key_to_pid)}
    ]);
dynamic_db_init(CacheNodes) ->
  % If other simple_cache instances are discovered, bring data over from the 
  % other nodes in the cluster.
  add_extra_nodes(CacheNodes).

-define(WAIT_FOR_TABLES, 5000).

%% ----------------------------------------------------------------------------
add_extra_nodes([Node|T]) ->
  io:format("Cloning mnesia tables! from Node: ~p~n", [Node]),
  % Tell Mnesia to add an extra node to the database. You need to connect to 
  % only one of the remote instances. Mnesia works in much the same way as 
  % Erlang nodes: when you connect to another Mnesia instance, you’re informed 
  % about the others, and the others are informed about you.
  %
  % Adding a node in this particular way should only be done with newly 
  % started nodes that are fully RAM-based and have an empty schema.
  case mnesia:change_config(extra_db_nodes, [Node]) of
    {ok, [Node]} ->
      % If the Node is successfully contacted...

      % This looks cheaty...
      %
      % mnesia:add_table_copy(schema, node(), ram_copies),
      % mnesia:add_table_copy(key_to_pid, node(), ram_copies),

      % Copy the mnesia schema of the specified Nodecto this node.
      mnesia:add_table_copy(schema, Node, ram_copies),
      % Copy the mnesia tables of the specified Nodecto this node.
      mnesia:add_table_copy(key_to_pid, Node, ram_copies),
      
      % Get a list of all known mnesia tables on this node, and, wait for the 
      % tables to be accessible.
      Tables = mnesia:system_info(tables),
      mnesia:wait_for_tables(Tables, ?WAIT_FOR_TABLES);
    _ ->
      % Failed to connect to specified node. Try the next node.
      %
      % If you run out of nodes to try to connect to, the code crashes, causing
      % the startup to fail, because you don’t handle the case when the list of 
      % nodes is empty. This is another example of “let it crash”; there is 
      % little point in adding code for that case.
      add_extra_nodes(T)
  end.



%% Checks whether a Pid refers to a process that still lives. If so, returns true.
%%
is_pid_alive(Pid) when node(Pid) =:= node() ->
  % If the Pid is running on this node then check if it is alive...
  is_process_alive(Pid);
is_pid_alive(Pid) ->
  % If the Pid is not running this node, but is in part of the (remote) cluster 
  % and...
  lists:member(node(Pid), nodes()) andalso
  % ... the process is alive on remote node...
  (rpc:call(node(Pid), erlang, is_process_alive, [Pid]) =:= true).




