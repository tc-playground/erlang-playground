%%%----------------------------------------------------------------------------
%%% The purpose of an active application is to run one or more processes to do 
%%% some work. In order to have control over those processes, they should be 
%%% spawned and managed by supervisors: processes that implement the supervisor 
%%% behaviour.
%%%
%%% Using the name <application-name>_sup is a common convention for the 
%%% module that implements the root supervisor behaviour for an application.
%%%
%%% @doc The OTP Root Superviser for the TCP-RPC Server
%%% @end
%%%----------------------------------------------------------------------------

-module(tr_sup).

%% Implements the OTP 'application' behaviour.
%%
-behaviour(supervisor).

%%-----------------------------------------------------------------------------
%% Public API 

-export([start_link/0]).

%%-----------------------------------------------------------------------------
%% Supervisor Behaviour Callbacks
-export([init/1]).

%% Definition Macros
-define(SERVER, ?MODULE).

%%-----------------------------------------------------------------------------

%% Start the superviser. Invoked from 'tr_app:start(Type, StartArgs)'.'
start_link() ->
  %% Call the 'superviser:start_link' to create a new linked process.
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% Initialisation involves creating the 'superviser specification' tuple.
init([]) ->

  %% Each process to be create during the init process should have a 'child-spec' 
  %% of the form:
  %%
  %% ChildSpec = {ID,Start,Restart,Shutdown,Type,Modules}
  %%
  %% Where:
  %%
  %% ID       - An term (atom) that the supervisor uses to identify the 
  %%            specification internally. e.g. 'tr_server'.
  %% Start    - A triple {Module, Function, Arguments} that is used to start the 
  %%            process.
  %% Restart  - Whether this is a child that should be restarted upon failure.
  %%            {permanent, temporary, transient}. 
  %% Shutdown - How the process may be killed. {timeout_ms, brutal_kill, infinity}
  %% Type     - Whether the process is a {supervisor, worker}.
  %% Modules  - The modules that this process depends on. This information is used 
  %%            only during hot code upgrades and indicates to the system in what 
  %%            order modules should be upgraded.
  %%

  %% We have a single 'child-spec' for the server.
  Server = {
    tr_server, 
    {tr_server, start_link, []},
    permanent, 
    2000, 
    worker, 
    [tr_server]
    },

  %% A list of 'child-specs'. One for each child process to be created. Here we 
  %% have one only...
  Children = [Server],

  %% The 'restart-strategy' is of the form:
  %%
  %% RestartStrategy = {How, Max, Within}
  %%
  %% Where:
  %%
  %% How      - Defines how a child process is restarted if it dies. The 
  %%            'one_for_one' strategy means if a child process dies, that a  
  %%            single process is spawned to replace it.
  %% Max      - The maximum number of restarts.
  %% Within   - The restart timeframe in seconds.
  %%
  %% Together New and Within defined the allowed 'restart frequency'.
  %%
  %% For instance, if Max=10 and Within=30, you allow at most 10 restarts within 
  %% any period of 30 seconds. If this limit is exceeded, the supervisor 
  %% terminates itself and all its child processes and propagates the failure up 
  %% the supervision tree.
  %%
  %% It is hard to recommend good all-round defaults, but 4 restarts per hour 
  %% (3600 seconds) is often used in production systems
  %%
  RestartStrategy = {one_for_one, 0, 1},

  %% Return the 'superviser specification'...
  {ok, {RestartStrategy, Children}}.



