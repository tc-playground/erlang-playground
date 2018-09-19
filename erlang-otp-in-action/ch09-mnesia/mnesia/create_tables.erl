
%%%============================================================================
%%% @doc 
%%% Create Mnesia tables for the 'simple_cache' project. 
%%% @end
%%%============================================================================

-module(create_tables).

%% Tables are created with the function mnesia:create_table(Name, Options), 
%% where Options is a list of {Name, Value} pairs. The main option that you 
%% almost always need to supply is attributes, which assigns names to the 
%% fields of the records that will be stored in the table. Without it, Mnesia 
%% assumes you’ll only have two fields in your records, named key and val, 
%% respectively.

%%%============================================================================
%%% Public API
%%%============================================================================

-export([init_tables/0]).

-export([insert_user/3, insert_project/2]).


%%%============================================================================
%%% Records
%%%============================================================================

%%  Mnesia tables and Erlang records
%% 
%% To Mnesia, a table is just a bunch of tagged tuples. This is exactly what 
%%  Erlang’s records are (see section 2.11), but Mnesia can’t know that a table 
%% has anything to do with your –record(...) declaration of the same name. 
%% You need to set up this connection yourself. (Sometimes, it can be useful 
%% not to be forced to have a connection between a table and a record, even if 
%% they have the same name.)
%% 
%% You could hardcode the names, as in {attributes, [title, description]}; 
%% but it’s better to use record_info(fields, RecordName) to list the field 
%% names, in case you change the record declaration later. Note that 
%% record_info/2 isn’t a real function — it will be resolved at compile time (
%% just like the # syntax for records) and can’t be called at runtime or from 
%% the Erlang shell.

%% No matter what you name them, the first field of the record is always the 
%% primary key.



-record(user, {id, name }).

-record(project, {title, description}).

-record(contributor, {user_id, project_title}).


%%%============================================================================
%%% Implemented API
%%%============================================================================


%% Only specifying the 'attributes' option means that the table will get the 
%% default settings for all other options. These are as follows:
%%
%% - The table is both readable and writeable.
%% - The table is stored in RAM only (the storage type is ram_copies).
%% - The records stored in the table must have the same name as the table.
%% - The table type is set, which means there can be no more than one entry
%%   per key.
%% - The load priority is 0 (the lowest).
%% - The local_content flag is set to false.
%%
init_tables() ->
  mnesia:create_table(
    user, [{attributes, record_info(fields, user)}]
    ),
  mnesia:create_table(
    project,[{attributes, record_info(fields, project)}]
    ),
  mnesia:create_table(
    contributor, [{type, bag}, {attributes, record_info(fields, contributor)}]
    ).




%% To make it straightforward for others to insert data without knowing too 
%% much about the actual tables, you’ll hide these details behind a couple of 
%% API functions. This also makes it possible to do some consistency checks 
%% on the data before it gets inserted in the database. For example, you’ll 
%% only add users if they’re contributors to some project, and you won’t 
%% allow adding a user for a project that doesn’t exist. 

%% Insert a new User and their associated Contributions to Projects.
%%
insert_user(Id, Name, ProjectTitles) when ProjectTitles =/= [] ->
  
  % Create a new User record
  User = #user{id = Id, name = Name},
  
  % Create an anonymous data insert function.
  Fun = fun() ->
    % Save the new User record to Mnesia.
    mnesia:write(User),
    % For each ProjectTitle...
    lists:foreach(
      fun(Title) ->
        % Look-up the Project record from the Title
        [#project{title = Title}] = mnesia:read(project, Title),
        % Sace a new Contributor record to Mnesia.
        mnesia:write(#contributor{user_id = Id, project_title = Title})
      end,
      ProjectTitles)
    end,
  
  % Perform all User/Contribution writes in an (ACID) transaction.
  mnesia:transaction(Fun).


%% Insert a new Project
%%
insert_project(Title, Description) ->
  % Create a new Project record and (dirty) write it to Mnesia.
  %
  % Any Mnesia function with the prefix 'dirty_' is a dirty operation that 
  % doesn’t respect transactions or database locks. This means it must be used 
  % with great care. Generally, using a dirty operation is significantly 
  % faster than setting up a transaction and performing normal database 
  % operations, and judicious use of dirty operations can speed up your 
  % application a lot. Be warned, though, you may end up with inconsistent 
  % data. Dirty reads are usu- ally less problematic than dirty writes; 
  % but whenever you’re in doubt, use transactions!
  %
  mnesia:dirty_write(#project{title=Title, description = Description}).




