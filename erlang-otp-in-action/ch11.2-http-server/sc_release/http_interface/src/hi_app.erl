%%%============================================================================
%%% @doc 
%%% The OTP App for the 'http_interface' application.
%%% @end
%%%============================================================================
-module(hi_app).

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

-define(DEFAULT_PORT, 1156).

%-define(DEFAULT_IP_ADDR, "127.0.0.1").

%%%============================================================================
%%% Public Interface Implementation
%%%============================================================================

%%-----------------------------------------------------------------------------
%% @doc Start the 'http_interface' application.
%%
%% @spec start(_StartType::any(), _StartArgs::any()) -> {ok, Other}
%% where
%%  ok = atom()
%%
%% @end
%%-----------------------------------------------------------------------------
start(StartType, StartArgs) ->

  io:format("Starting HI_APP: ~p - ~p~n", [StartType, StartArgs]),

    % Get the TCP port configuration or use the defalt port.
  % IP = case application:get_env(http_interface, ip_addr) of
  %   {ok, P}   -> P;
  %   undefined -> ?DEFAULT_IP_ADDR
  %   end,

  % Get the TCP port configuration or use the default port.
  Port = 
    case application:get_env(http_interface, port) of
      {ok, P}   -> P;
      undefined -> ?DEFAULT_PORT
    end,

  % hi_sup:start_link(IP_ADDR, Port).
  hi_sup:start_link(Port).

  % case hi_sup:start_link(Port) of
  %   {ok, Pid} ->
  %     {ok, Pid};
  %   Other ->
  %       {error, Other}
  % end.



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





