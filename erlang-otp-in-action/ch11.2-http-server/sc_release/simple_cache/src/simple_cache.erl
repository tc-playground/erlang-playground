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
%%% NB: This module has been augmented with the 'sc_event' GenEvent emitter.
%%% Bu putting the event emitter here we don't couple it tightly to the 
%%% 'sc_element' and 'sc_store' modules... (I wonder how you could wire this
%%% this stuff in though... sort of Aspect-Oriented...).
%%%
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
  % NB: How to make this configurable? IoC like / Protocol like?
  % case sc_store:lookup(Key) of
  case sc_store:lookup(Key) of
    {ok, Pid} ->
      Res = sc_element:replace(Pid, Value),
      sc_event:replace(Key, Value),
      Res;
    {error, _} ->
      % Create
      {ok, Pid} = sc_element:create(Value),
      sc_event:create(Key, Value),
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
      Res = sc_element:replace(Pid, Value),
      sc_event:replace(Key, Value),
      Res;
    {error, _} ->
      % Create
      {ok, Pid} = sc_element:create(Value, LeaseTime),
      sc_event:create(Key, Value, LeaseTime),
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
    io:format("Found Pid:~p~n", [Pid]),
    sc_event:lookup(Key),
    {ok, Value} = sc_element:fetch(Pid),
    io:format("Fetched Value:~p~n", [Value]),
    {ok, Value}
  catch
    _Class:_Exception ->
      io:format("Exception:~p:~p~n", [_Class, _Exception]),
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
      sc_store:delete(Pid),
      Res = sc_element:delete(Pid),
      sc_event:delete(Key),
      Res;
    {error, _Reason} ->
      ok
  end.



