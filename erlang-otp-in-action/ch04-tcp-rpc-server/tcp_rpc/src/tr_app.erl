%%%----------------------------------------------------------------------------
%%% Every active application needs one module that implements the application 
%%% behaviour. This module provides the startup logic for the system. 
%%%
%%% At a minimum, it provides the point from which the root supervisor is 
%%% started; that supervisor is the grandparent of all the processes that will 
%%% be part of the application. 
%%% 
%%% The application behaviour module may also do other things depending on the 
%%% needs of your system.
%%%
%%% A running application is essentially a tree of processes, both supervisors 
%%% and workers, where the root of the tree is the root supervisor. 
%%%
%%% Using the name <application-name>_app is a common convention for the 
%%% module that implements the application behaviour.
%%%
%%% @doc The OTP App for the TCP-RPC Server
%%% @end
%%%----------------------------------------------------------------------------
-module(tr_app).

%%% Implements the OTP 'application' behaviour.
%%%
-behaviour(application).


%%% Exports the 'application behvaiour' callbacks.
%%%
-export([
  start/2,
  stop/1
  ]).


%%%----------------------------------------------------------------------------
%% This is called when the OTP system wants to start your application, and it 
%% must perform the actual startup and return the process ID of the root 
%% supervisor as {ok, Pid}. 
%%
%% You can do any other startup tasks here as well, such as read a configuration 
%% file, initialize an ETS table, and so on
%%
%% _Type      :: {normalm failover, takeover, ... , etc}
%% _StartArgs :: Whatever arguments you specify in the mod parameter in the 
%%               .app file.
%%
start(_Type, _StartArgs) ->

  %% Call the 'tr_sup' OTP 'root superviser' module to init the system and 
  %% (if successfull) return the PId of the created root superviser process.
  %%
  case tr_sup:start_link() of

    %% Success! Return the PId of the created root superviser process.
    %%
    {ok, RootSuperviserPid} ->
          {ok, RootSuperviserPid};
      
    %% Error!
    %%
    Other ->
      {error, Other}
end.


%% Don’t need to do anything special on shutdown of the tcp-rpc server, soignore 
%% the input parameter and return the atom ok. 
%%
%% _State :: The app state.
%%
stop(_State) ->
  ok.