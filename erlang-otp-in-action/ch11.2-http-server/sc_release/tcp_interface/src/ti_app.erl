%%%============================================================================
%%% @doc 
%%% The OTP App for the 'tcp_interface' application.
%%% @end
%%%============================================================================
-module(ti_app).

%%%============================================================================
%%% OTP Application Behaviour
%%%============================================================================

%% THE APPLICATION BEHAVIOUUR
%%
%% Aan interface for starting and stopping the application; but there is also  
%% an associated behaviour container, known as the application controller,  
%% which handles all the applications running in the system.
%%
%% When the application start/2 callback function has completed the startup, 
%% it should return the pid of the newly started top-level supervisor so that 
%% the container can track it. In this way, the application behaviour is 
%% similar to the gen_event behaviour: a single container manages multiple
%% behaviour implementations.
%%
%%
%% THE APPLICATION CONTROLLER
%%
%% There is only one application controller per runtime system, registered under 
%% the name 'application_controller' (erl> registered().).
%%
%% The controller is also responsible for loading the .app file for the 
%% application and checking that all the applications it depends on have been 
%% started first. For each running application, a pair of application master 
%% processes are spawned by the application controller in order to isolate 
%% itself from the application code.
%%
%%
%% APPLICATION START TYPES
%%
%% When an application is started using application:start(AppName), it gets the 
%% default type 'temporary'. This means even if it terminates unexpectedly, 
%% the rest of the runtime system isn’t affected; only a crash report is 
%% generated. an application that is started through a call to 
%% application:start(AppName, permanent) is considered required for the target 
%% system to function: if it terminates, for whatever reason, the entire 
%% runtime system shuts down so that everything can be restarted from scratch.
%%
%% Such a system restart can be handled automatically by an external operating 
%% system heart process; see the Erlang/OTP documentation of the heart module 
%% (part of the kernel application) for more details.
%%
-behaviour(application).


%%%============================================================================
%%% Public Interface
%%%============================================================================

-export([start/2, stop/1]).


%%%============================================================================
%%% Macros
%%%============================================================================

-define(DEFAULT_PORT, 1155).

%%%============================================================================
%%% Public Interface Implementation
%%%============================================================================

%%-----------------------------------------------------------------------------
%% @doc Start the 'tcp interface' application.
%%
%% @spec start(_StartType::any(), _StartArgs::any()) -> {ok, Other}
%% where
%%  ok = atom()
%%
%% @end
%%-----------------------------------------------------------------------------
start(StartType, StartArgs) ->

  io:format("Starting TI_APP: ~p - ~p~n", [StartType, StartArgs]),

  % Get the TCP port configuration or use the defalt port.
  Port = case application:get_env(tcp_interface, port) of
    {ok, P}   -> P;
    undefined -> ?DEFAULT_PORT
    end,

  % Attempts ot listen on the specified port.
  %
  % The listening socket was opened in active mode.
  %
  % {ok, LSock} = gen_tcp:listen(Port, [{active, true}]),

  case gen_tcp:listen(Port, [{active, true}]) of 

    {ok, LSock} -> 
      io:format("Bound TCP Socket on Port: ~p~n", [Port]),
      % Start the initial 'application root superviser' handler with the specified
      % socket.
      case ti_sup:start_link(LSock) of
        {ok, Pid} ->
          ti_sup:start_child(),
          {ok, Pid};
        Other ->
          {error, Other}
      end;

    {error,eaddrinuse} ->
      io:format("Could not bind TCP socket on Port: ~p~n", [Port]),
      io:format("Not starting ~p~n", [?MODULE]),
      io:format("Meh.......~n"),
      {error, normal}

  end.



%%-----------------------------------------------------------------------------
%% @doc Stop the 'tcp interface' application.
%%
%% @spec stop() -> ok
%% where
%%  ok = atom()
%%
%% @end
%%-----------------------------------------------------------------------------
stop(_State) ->
  ok.


%%%============================================================================
%%% Private Functions
%%%============================================================================





