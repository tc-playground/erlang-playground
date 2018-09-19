%%%============================================================================
%%% @doc 
%%% A storage mechanism that maps 'cache keys' to the 'process identifiers' of 
%%% the 'cache values' that ares tored against them.
%%%
%%% Erlang Term Storage is used to implemented this mapping.
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
%%% Public Interface Implementation
%%%============================================================================

%% Creates a new ETS table named 'sc_store'.
init() ->
  % Create a new 'ets named table' called 'sc_store'.
  ets:new(?TABLE_ID, [public, named_table]),
  ok.


insert(Key, Pid) ->
  % In ETS there can only be one entry at a time for any specific key, so  
  % inserting a new tuple using an existing key overwrites the previous entry.
  ets:insert(?TABLE_ID, {Key, Pid}).


lookup(Key) ->
  % Look up the tuple referenced by the key in the ETS table.
  case ets:lookup(?TABLE_ID, Key) of
    [{Key, Pid}] 
      -> {ok, Pid};           % If it exists return the required Pid.
    []           
      -> {error, not_found}   % If not, return the atom 'not_found'
  end.


delete(Pid) ->
  % ETS tables have a powerful mechanism for using patterns to search through tables 
  % without extracting every entry. This still scans the whole table but is fast 
  % because it’s all done from C code with minimal copying of data.
  %
  ets:match_delete(?TABLE_ID, {'_', Pid}).


%% *** Match patterns ***
%%
%% These are patterns expressed as Erlang terms, and they can consist of three things:
%%
%% - Normal Erlang terms and already-bound variables.
%%
%% - An underscore as a single-quoted atom ('_'). This has the same meaning
%%   as an underscore in a regular Erlang pattern—that is, a don’t-care pattern
%%   or wildcard.
%%
%% - Pattern variables of the form '$<integer>' (for example, '$1', '$2', '$3', ...).
%%
%%
%% For example, given a stored tuple of the form {erlang,number,1}, a pattern like 
%% {erlang,'_',1} will match it. The '_' wildcard indicates that you don’t care what 
%% is in this position. This pattern will match any 3-tuple with the atom erlang as the 
%% first element and the integer 1 as the last. You can also use pattern variables to 
%% retrieve values selectively from matched tuples; for instance, a match pattern 
%% like {'$2','$1','_'} (on the same stored tuple) will yield the list [number, erlang] 
%% because of how the fields line up and because the values of the pattern variables 
%% are always returned in the order they’re numbered. See the documentation for 
%% ets:match/2 for details.




