%%%============================================================================
%%%
%%% @doc 
%%% The OTP 'Root Supervisor' for the 'tcp_interface' application.
%%%
%%% This short module provides a nice, OTP-compliant factory for the ti_server 
%%% processes that handle the incoming connections.
%%% @end
%%%============================================================================
-module(ti_sup).


%%%============================================================================
%%% OTP Supervisor Behaviour
%%%============================================================================

-behaviour(supervisor).

-export([init/1]).


%%%============================================================================
%%% Public API
%%%============================================================================

-export([start_link/1, start_child/0]).


%%%============================================================================
%%% Macros
%%%============================================================================

-define(SERVER, ?MODULE).


%%%============================================================================
%%% Public API Implementation
%%%============================================================================

start_link(LSock) ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, [LSock]).

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
init([LSock]) ->

  % Define the 'tl_server' init process.
  %
  TLServerSpec = {
    ti_server,                          % The Id to identify the child specification.
    {ti_server, start_link, [LSock]},   % The apply(M, F, A) tuple to start the process.
    temporary,                          % Child process always restarted.
    brutal_kill,                        % Terminate child: immediately
    worker,                             % Child is a worker process.
    [ti_server]                         % The name of the callback module.
  },


  ManagedProcSpecs = [TLServerSpec],

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



