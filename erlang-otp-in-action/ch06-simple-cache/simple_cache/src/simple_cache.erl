%%%============================================================================
%%% @doc 
%%%
%%% An API to the simple_cache appli- cation. This module is a set of interface 
%%% functions for clients of the  'simple_cache':
%%%
%%% - insert/2—Stores a key and corresponding value in the cache.
%%%
%%% - insert/3—Stores a key and corresponding value in the cache with the 
%%%   specified LeaseTime,.
%%%
%%% - lookup/1—Uses a key to retrieve a value.
%%%
%%% - delete/1—Uses a key to delete the key/value pair from the cache.
%%%
%%% NB: This API doesn’t include any functions for starting or stopping the 
%%% simple_cache; that is handled via OTP system functions such as 
%%% application:start/1.
%%%
%%% @end
%%%============================================================================

%% NB: The convention for application-level API modules is to give them the same 
%% name as the application.
-module(simple_cache).


%%%============================================================================
%%% Public Interface
%%%============================================================================


-export([insert/2, insert/3, lookup/1, delete/1]).



%%%============================================================================
%%% Public Interface Implementation
%%%============================================================================

%% @doc
%% Takes a key and a value and stores the pair in the cache with the 
%% default LeaseTime.
%% 
%% Return {noreply, State} tuple (if 'replacing' a value), or, the atom 
%% true (if 'inserting' a new Value).
%% @end
insert(Key, Value) ->
  case sc_store:lookup(Key) of
    {ok, Pid} ->
      sc_element:replace(Pid, Value);
    {error, _} ->
      {ok, Pid} = sc_element:create(Value),
      sc_store:insert(Key, Pid)
  end.


%% @doc
%% Takes a key and a value and stores the pair in the cache with the 
%% specified LeaseTime. 
%% 
%% Return {noreply, State} tuple (if 'replacing' a value), or, the atom 
%% true (if 'inserting' a new Value).
%% @end
insert(Key, Value, LeaseTime) ->
  case sc_store:lookup(Key) of
    {ok, Pid} ->
      sc_element:replace(Pid, Value);
    {error, _} ->
      {ok, Pid} = sc_element:create(Value, LeaseTime),
      sc_store:insert(Key, Pid)
  end.


%% @doc
%% Takes a key and returns the associated Value ins an {ok, Value} 
%% tuple; or returns an  {error, not_found} tuple.
%% @end
lookup(Key) ->
  % Using a try expression like this can be useful whenever you have a 
  % sequence of things that must be done in order and the result should 
  % be the same if any of the steps fails.
  try
    {ok, Pid} = sc_store:lookup(Key),
    {ok, Value} = sc_element:fetch(Pid),
    {ok, Value}
  catch
    _Class:_Exception ->
    {error, not_found}
  end.

%% @doc
%% Takes a key and terminates the associated process holding the Value state.
%%
%% Returns a {stop, normal, State} tuple if an existing process was 
%% terminated; or the atom ok if it did not exists.
%% @end
delete(Key) ->
  case sc_store:lookup(Key) of
    {ok, Pid} ->
      sc_element:delete(Pid);
    {error, _Reason} ->
      ok
  end.