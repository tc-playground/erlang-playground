%%%============================================================================
%%% @doc 
%%% The processes that store 'cached values' in the 'simple_cache' application.
%%%
%%% Each cached value is stored as a 'sc_element' GenServer process
%%% @end
%%%============================================================================
-module(sc_element).

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
%%% Public Interface
%%%============================================================================

-export([
  start_link/2,
  create/2,
  create/1,
  fetch/1,
  replace/2,
  delete/1
]).

%% Constant
%%
-define(SERVER, ?MODULE).

%% Constant - The default time (one day in seconds) a cached value can remain in 
%% the cahce.
%%
-define(DEFAULT_LEASE_TIME, (60 * 60 * 24)).


%%%============================================================================
%%% Private State
%%%============================================================================

%% Record  - Holds the cached value.
%% value       - The value the process is holding on to, 
%% lease_time  - The lease time
%% timestamp   - The timestamp from  when the process was started.
%%
-record(state, {value, lease_time, start_time}).


%%%============================================================================
%%% Public Interface Implementation
%%%============================================================================

%% Create a new 'tc_element' GenServer process.
%%
%% NB: This method is called from the 'sc_element_sup' supervisor to allow the  
%%     management/supervision of created processes.
%%
start_link(Value, LeaseTime) ->
    gen_server:start_link(?MODULE, [Value, LeaseTime], []).

%% Create (store) a new Value with the specified LeaseTime.
%%
%% NB: This method calls the 'sc_element_sup' supervisor to allow the  
%%     management/supervision of created processes. 
%%
%%     The supervisor will use the 'sc_element:start_link(Value, LeaseTime)' 
%%     (configured in the 'sc_sup:init' method) to actually create the  
%%     supervised process. This method in turn call the GenServer behviour 
%%     'start_link' message. Simples! 
%% 
create(Value, LeaseTime) ->
    sc_element_sup:start_child(Value, LeaseTime).

%% Create (store) a new Value with the default LeaseTime. 
create(Value) ->
    create(Value, ?DEFAULT_LEASE_TIME).

%% Fetch the state Value of the specified storage process. 
fetch(Pid) ->
  % Synchronously calls the specified GenServer process with a 'fetch' message  
  % to return the GenServer process 'state' (the stored 'Value').
  gen_server:call(Pid, fetch).

%% Replace the state Value of the specified storage process. 
replace(Pid, Value) ->
  % Asynchronously calls the specified GenServer process with a 'replace'   
  % message to replace the specified Value.
  gen_server:cast(Pid, {replace, Value}).

%% Delete the state Value of the specified storage process. 
delete(Pid) ->
  % Asynchronously calls the specified GenServer process with a 'delete'   
  % message to delete the specified Value.
  gen_server:cast(Pid, delete).


%%%============================================================================
%%% OTP GenServer Callbacks
%%%============================================================================

%%% Setting server timeouts
%%%
%%% Remember that if you forget to return a new timeout value in one of the 
%%% callback functions, the timeout will revert to infinity. When you’re using 
%%% server timeouts, it’s important to remember to set them in every clause of 
%%% every callback function.
%%%

init([Value, LeaseTime]) ->
  Now = calendar:local_time(),
  % Gregorian seconds: a useful uniform representation of time as the number 
  % of seconds since year 0 (year 1 BC) according to the normal 
  % Western/International Gregorian calendar.
  StartTime = calendar:datetime_to_gregorian_seconds(Now),

  % If the server process isn’t accessed within the lease period, a timeout 
  % message is sent to the server and passed to the handle_info/2 function, 
  % which shuts down the process.

  {ok,                              % ok
    #state{                         % State
      value = Value, 
      lease_time = LeaseTime, 
      start_time = StartTime
    }, 
    time_left(StartTime, LeaseTime) % The server timeout after initialization
  }.

% Handle 'fetch' message.
handle_call(fetch, _From,  State) ->
  % Get State.
  #state{value = Value, lease_time = LeaseTime, start_time = StartTime} = State,
  % Compute timeout..
  TimeLeft = time_left(StartTime, LeaseTime),
  {reply, {ok, Value}, State, TimeLeft}.


% Handle 'replace' message.
handle_cast({replace, Value}, State) ->
  #state{lease_time = LeaseTime, start_time = StartTime} = State,
  TimeLeft = time_left(StartTime, LeaseTime),
  % Returns 'noreply', meaning that the server doesn’t reply, but, stays alive.
  {noreply, State#state{value = Value}, TimeLeft};


% Terminate this sc_element process, removing it from the cache.
handle_cast(delete, State) ->
  % Returns 'stop', meaning that the server doesn’t reply and terminates..
  {stop, normal, State}.

% Kill/Stop the process.
handle_info(timeout, State) ->
  {stop, normal, State}.


% Delete from store
terminate(_Reason, _State) ->
  % sc_store:delete(self()),
  sc_store_mnesia:delete(self()),
  ok.

% Code change.
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.


%%%============================================================================
%%% Private Functions 
%%%============================================================================

%% Computes the number of milliseconds left of the lease.
%% NB: System uses time in milliseconds, not, seconds.
%%
time_left(_StartTime, infinity) ->
  infinity;
time_left(StartTime, LeaseTime) ->
  
  Now = calendar:local_time(),
  
  CurrentTime = calendar:datetime_to_gregorian_seconds(Now),
  
  TimeElapsed = CurrentTime - StartTime,
  
  case LeaseTime - TimeElapsed of
    Time when Time =< 0 
      -> 0;
    Time 
      -> Time * 1000
  end.


