%%%============================================================================
%%%
%%% @doc The OTP App for the 'resource_discovery' application.
%%% @end
%%%============================================================================
-module(rd_app).

%%%============================================================================
%%% OTP Application Behaviour
%%%============================================================================

-behaviour(application).


%%%============================================================================
%%% Public Interface
%%%============================================================================

-export([start/2, stop/1]).


%%-----------------------------------------------------------------------------
%% @doc Start the 'resource_discovery' application.
%%
%% @spec start(_StartType::any(), _StartArgs::any()) -> {ok, Other}
%% where
%%  ok = atom()
%%
%% @end
%%-----------------------------------------------------------------------------
start(_StartType, _StartArgs) ->

  % Start the 'application root superviser'
  case rd_sup:start_link() of

    {ok, RootSupervisorPid} ->
      {ok, RootSupervisorPid};

    Other ->
      {error, Other}

  end.

%%-----------------------------------------------------------------------------
%% @doc Stop the 'resource_discovery' application.
%%
%% @spec stop() -> ok
%% where
%%  ok = atom()
%%
%% @end
%%-----------------------------------------------------------------------------
stop(_State) ->
  ok.
