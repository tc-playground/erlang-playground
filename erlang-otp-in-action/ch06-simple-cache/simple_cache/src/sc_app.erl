%%%============================================================================
%%%
%%% @doc The OTP App for the 'simple_cache' application
%%% @end
%%%============================================================================
-module(sc_app).

%%%============================================================================
%%% OTP Application Behaviour
%%%============================================================================

-behaviour(application).


%%%============================================================================
%%% Public Interface
%%%============================================================================

-export([start/2, stop/1]).


%%-----------------------------------------------------------------------------
%% @doc Start the 'simple cache' application.
%%
%% @spec start(_StartType::any(), _StartArgs::any()) -> {ok, Other}
%% where
%%  ok = atom()
%%
%% @end
%%-----------------------------------------------------------------------------
start(_StartType, _StartArgs) ->

  % Initialise the 'sc_store' module (currently an ETS table.)
  sc_store:init(),

  % Start the 'application root superviser'
  case sc_sup:start_link() of

    {ok, RootSupervisorPid} ->
      {ok, RootSupervisorPid};

    Other ->
      {error, Other}
  end.

%%-----------------------------------------------------------------------------
%% @doc Stop the 'simple cache' application.
%%
%% @spec stop() -> ok
%% where
%%  ok = atom()
%%
%% @end
%%-----------------------------------------------------------------------------
stop(_State) ->
  ok.




