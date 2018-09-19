%%%============================================================================
%%%
%%% @doc 
%%% The OTP 'Root Supervisor' for the 'simple_cache' application.
%%%
%%% This supervisor is in many ways just a factory for 'sc_element' processes.
%%% @end
%%%============================================================================
-module(sc_sup).


%%%============================================================================
%%% OTP Supervisor Behaviour
%%%============================================================================

-behaviour(supervisor).

-export([init/1]).


%%%============================================================================
%%% Public API
%%%============================================================================

-export([start_link/0]).


%%%============================================================================
%%% Macros
%%%============================================================================

-define(SERVER, ?MODULE).


%%%============================================================================
%%% Public API Implementation
%%%============================================================================

start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).


%%%============================================================================
%%% OTP Supervisor Callbacks
%%%============================================================================

%% Initialise the RootSuperviser process with two child processes:
%% 1) 'sc_element_sup' : The 'supervisor process' managing 'sc_element' processes.
%% 2) 'sc_event'       : The event manager process.
%%
%% This is creating a 'Superviser Tree Hierarhcy'. Following this pattern, you 
%% can nest supervisors to any depth you want to give your application a suitably 
%% fine-grained supervision structure.
%%
init([]) ->

  % Define the 'sc_element_sup' supervisor process.
  %
  SCElementSup = {
    sc_element_sup,                     % The Id to identify the child specification.
    {sc_element_sup, start_link, []},   % The apply(M, F, A) tuple to start the process.
    permanent,                          % Child process always restarted.
    2000,                               % Terminate child: 'exit(Child, shutdown)' timeout.
    supervisor,                         % Child is supervisor process.
    [sc_element]                        % The name of the callback module.
  },

  % Define the 'sc_event' event process.
  %
  EventManager = {
    sc_event,                           % The Id to identify the child specification.
    {sc_event, start_link, []},         % The apply(M, F, A) tuple to start the process.
    permanent,                          % Child process always restarted.
    2000,                               % Terminate child: 'exit(Child, shutdown)' timeout.
    worker,                             % Child is supervisor process.
    [sc_event]                          % The name of the callback module.
  },

  Children = [SCElementSup, EventManager],

  % Strategy : 'one_for_one'
  % 
  % If a child process terminates, only that process is restarted.
  %
  %
  % Maximum Restart Frequency : (4, 3600)
  % 
  % If more than '4' restarts occur in the last '3600' seconds, then the supervisor 
  % terminates all the child processes and then itself.
  %
  RestartStrategy = {one_for_one, 4, 3600},

  {ok, {RestartStrategy, Children}}.
