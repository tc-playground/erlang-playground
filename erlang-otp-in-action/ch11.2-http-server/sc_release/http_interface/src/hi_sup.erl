%%%============================================================================
%%%
%%% @doc 
%%% The OTP 'Root Supervisor' for the 'http_interface' application.
%%%
%%% This short module provides a nice, OTP-compliant factory for the hi_server 
%%% processes that handle the incoming connections.
%%% @end
%%%============================================================================
-module(hi_sup).


%%%============================================================================
%%% OTP Supervisor Behaviour
%%%============================================================================

-behaviour(supervisor).

-export([init/1]).


%%%============================================================================
%%% Public API
%%%============================================================================

-export([
  start_link/1, 
  % start_link/2, 
  start_child/0
]).


%%%============================================================================
%%% Macros
%%%============================================================================

-define(SERVER, ?MODULE).


%%%============================================================================
%%% Public API Implementation
%%%============================================================================

start_link(Port) ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, [Port]).

% start_link(IP, Port) ->
%   supervisor:start_link({local, ?SERVER}, ?MODULE, [IP, Port]).

start_child() ->
  supervisor:start_child(?SERVER, []).


%%%============================================================================
%%% OTP Supervisor Callback Implementation
%%%============================================================================



%% The init/1 callback function gets the listening socket and includes it in the 
%% child spec so it becomes an argument to each new child.
%%
%% This is creating a 'Superviser Tree Hierarhcy'. Following this pattern, you 
%% can nest supervisors to any depth you want to give your application a suitably 
%% fine-grained supervision structure.
%%
init([Port]) ->

  % Define the 'tl_server' init process.
  %
  HLServerSpec = {
    hi_server,                          % The Id to identify the child specification.
    {hi_server, start_link, [Port]},    % The apply(M, F, A) tuple to start the process.
    permanent,                          % Child process always restarted.
    2000,                               % Terminate child: immediately
    worker,                             % Child is a worker process.
    [hi_server]                         % The name of the callback module.
  },


  ManagedProcSpecs = [HLServerSpec],

  % Strategy : 'one_for_one'
  % 
  % If a child process terminates, only that process is restarted.
  %
  % Maximum Restart Frequency : (4, 3600)
  % 
  % If more than '1' restarts occur in the last '0' seconds, then the supervisor 
  % terminates all the child processes and then itself.
  %
  ProcRestartStrategy = {one_for_one, 0, 1},

  {ok, {ProcRestartStrategy, ManagedProcSpecs}}.



