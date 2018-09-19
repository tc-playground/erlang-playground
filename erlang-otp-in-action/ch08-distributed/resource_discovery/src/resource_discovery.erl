%%%============================================================================
%%% @doc 
%%% A resource discovery application that functions a bit like the yellow pages. 
%%% 
%%% Each node in the cluster runs a local instance of this application. Each 
%%% such instance discovers and caches information about the available resources 
%%% in the cluster. This distributed, dynamic approach makes the system flexible 
%%% and powerful for a number of reasons:
%%% 
%%% - No single point of failure: It’s a peer-to-peer system.
%%% 
%%% - No hardcoded network topology: You can add resources where you want them.
%%% 
%%% - Easier scaling: You can add more resources as needed.
%%% 
%%% - Ability to run many services in a single node: Discovery is location 
%%%   transparent and works just as well with only one node (particularly good 
%%%   for testing).
%%% 
%%% - Easier upgrades: You can bring down old services and start new ones 
%%%   dynamically. Removed services become unregistered, and new ones are 
%%%   discovered as they come online.
%%% 
%%% @end
%%%============================================================================
-module(resource_discovery).


%%%============================================================================
%%% OTP GenServer Behaviour
%%%============================================================================

-behaviour(gen_server).

-export([
  init/1, 
  handle_call/3, 
  handle_cast/2, 
  handle_info/2,
  terminate/2, 
  code_change/3
]).


%%%============================================================================
%%% Public API
%%%============================================================================

-export([
  start_link/0,
  add_target_resource_type/1,
  add_local_resource/2,
  fetch_resources/1,
  trade_resources/0
]).


%%%============================================================================
%%% Macros
%%%============================================================================

-define(SERVER, ?MODULE).


%%%============================================================================
%%% State Record
%%%============================================================================

-record(
  state,  {
    target_resource_types,  % “I want spec”. <List::Type>.
    local_resource_tuples,  % “I have spec”. <Dict::{Type, Instance}>.
    found_resource_tuples   % "Cached remote specs". <Dict>.
    }
  ).


%%%============================================================================
%%% Public API Implementation
%%%============================================================================

%% Standard 'start_link' implementation. register the server process locally so 
%% it can be easily found by name, even from a remote node.
%%
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% Add a 'target_resource_types' element.
%%
add_target_resource_type(Type) ->
  gen_server:cast(?SERVER, {add_target_resource_type, Type}).

%% Add a 'add_local_resource' element.
%%
add_local_resource(Type, Instance) ->
  gen_server:cast(?SERVER, {add_local_resource, {Type, Instance}}).

%% A synchronous call, asking for a list of all the resource instances you’re 
%% looking for and know about for a given resource type.
%%
fetch_resources(Type) ->
  gen_server:call(?SERVER, {fetch_resources, Type}).

%% A synchronous call, asking for a list of all the resource instances you’re 
%% looking for and know about for a given resource type.
%%
trade_resources() ->
  gen_server:cast(?SERVER, trade_resources).


%%%============================================================================
%%% GenServer Implementation Callbacks
%%%============================================================================

%% Defines your initial server state: the target_resource_types field is 
%% initially an empty list, whereas found_resource_tuples and 
%% local_resource_tuples are empty dictionaries (associative arrays), using the 
%% standard library dict module
%%
init([]) ->
  {
    ok, 
    #state{
      target_resource_types = [],
      local_resource_tuples = dict:new(),
      found_resource_tuples = dict:new()
    }
  }.


%% Call dict:find/2 to look up Type in the current resources. This yields either 
%% {ok, Value} or error, which happens to be what you want the fetch_resources/1 
%% function to return, so there’s no need to massage the result — you can pass 
%% it straight back.
%%
handle_call({fetch_resources, Type}, _From, State) ->
  {reply, dict:find(Type, State#state.found_resource_tuples), State}.



%% Prepend the new Type to the current list, first performing a delete so that 
%% you avoid duplicate entries.
%%
handle_cast({add_target_resource_type, Type}, State) ->
  TargetTypes = State#state.target_resource_types,
  NewTargetTypes = [Type | lists:delete(Type, TargetTypes)],
  {noreply, State#state{target_resource_types = NewTargetTypes}};
%% Prepend the new {Type, Instance} to the current list, first performing a 
%% delete so that you avoid duplicate entries.
%%
handle_cast({add_local_resource, {Type, Instance}}, State) ->
  ResourceTuples = State#state.local_resource_tuples,
  NewResourceTuples = add_resource(Type, Instance, ResourceTuples),
  {noreply, State#state{local_resource_tuples = NewResourceTuples}};
%% The trade_resources message tells the local resource discovery server to 
%% broadcast messages asynchronously to each of the resource discovery servers 
%% on all the connected nodes in the Erlang cluster (including the local 
%% node itself, for a nice symmetry that lets you update your local list of 
%% matching resources without any additional code).
%%
%% Note: 
%% Using the synchronous gen_server:call/3 for such purposes is generally a bad 
%% idea: doing so would make this server block until the remote server replied 
%% and if the remote server was busy trying to call this server, you’d have a 
%% deadlock on your hands.
%%
handle_cast(trade_resources, State) ->
  % Dereference the  List::{Type, Instance} tuples.
  ResourceTuples = State#state.local_resource_tuples,
  % For each known node in the cluster...
  AllNodes = [node() | nodes()],
  lists:foreach(
    fun(Node) ->
      % See below for {trade_resources, {ReplyTo, Resources}}... handling
      % function..
      gen_server:cast(
        {?SERVER, Node},
        {trade_resources, {node(), ResourceTuples}}
        )
    end,
    AllNodes
  ),
  {noreply, State};
%% These broadcast messages have the form {trade_resources, {ReplyTo, Resources}}, 
%% where ReplyTo is the node name of the sender (given by node()), and Resources 
%% is the entire data structure (a dict) that holds the current resource tuples 
%% that the sender is publishing. Note that you don’t need to worry about the 
%% receiving process mucking up your local data structure, because message 
%% passing is strictly by copy, and because Erlang allows you to send any data 
%% in messages—there’s no need to rewrite or marshal the data—you can 
%% include the dictionary as it is in the message.
handle_cast(
    {trade_resources, {ReplyTo, Remotes}},
    % Alias pattern: it both matches and assigns a name at the same time...
    #state{local_resource_tuples = Locals,  
    target_resource_types = TargetTypes,
    found_resource_tuples = OldFound} = State
    ) ->

  FilteredRemotes = resources_for_types(TargetTypes, Remotes),
  NewFound = add_resources(FilteredRemotes, OldFound),

  %% The reply has the same shape as the broadcast message, but instead of 
  %% the sender’s node name it uses the atom noreply to indicate that no 
  %% further reply is needed—otherwise, messages would bounce back and forth 
  %% forever.
  case ReplyTo of
    noreply ->
      % Return ok atom. We dont want to bounce back and fourth forever!
      ok;
    _ ->
      % Otherwise, trade this nodes local resources back to the requester
      % to complete the 'trade'...
      gen_server:cast({?SERVER, ReplyTo},
      {trade_resources, {noreply, Locals}})
  end,

  {noreply, State#state{found_resource_tuples = NewFound}}.


handle_info(ok = _Info, State) ->
  {noreply, State}.


terminate(_Reason, _State) ->
  ok.


code_change(_OldVsn, State, _Extra) ->
  {ok, State}.




%%%============================================================================
%%% Private Internal Functions
%%%============================================================================

%% Add a new Resource of the specified Type to the specified list of 
%% ResourceTuples.
%%
add_resource(Type, Resource, ResourceTuples) ->
  case dict:find(Type, ResourceTuples) of
    {ok, ResourceList} ->
      %% If a ResourceList of the specified Type exists in ResourceTuples; 
      %% add the new Resource to the ResourceList and update the ResourceTuples. 
      NewList = [Resource | lists:delete(Resource, ResourceList)],
      dict:store(Type, NewList, ResourceTuples);
    error ->
      %% If not create a new ResourceList add the new Resource and update the 
      %% ResourceTuples. 
      dict:store(Type, [Resource], ResourceTuples)
  end.


%% Add a list of resource tuples.
%%
add_resources([{Type, Resource}|T], ResourceTuples) ->
  add_resources(T, add_resource(Type, Resource, ResourceTuples));
add_resources([], ResourceTuples) ->
  ResourceTuples.

%% Goes over a list of types (using lists:foldl/3), building a total list of 
%% all resources you know about for the given type.
%%
resources_for_types(Types, ResourceTuples) ->
  Fun = fun(Type, Acc) ->
            case dict:find(Type, ResourceTuples) of
              {ok, List} ->
                % Creates list of pairs...
                [{Type, Instance} || Instance <- List] ++ Acc;
              error -> Acc
            end
        end,

  lists:foldl(Fun, [], Types).




