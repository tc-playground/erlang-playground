%% ----------------------------------------------------------------------------
%% File: gen_server_skel.erl
%%
%% This is a skeleton implementation of a gen_server callback module.


%% ----------------------------------------------------------------------------
%% Behaviours
%% 
%% The whole point of a behaviour is to provide a template for processes of a 
%% particular type. Every behaviour library module has one or more API functions 
%% (generally called start and/or start_link) for starting a new container 
%% process.
%% 
%% We call this instantiat ing the behaviour.
%%


%% ----------------------------------------------------------------------------
%% Components of a Behaviour
%% 
%% There are 3 main components to a behaviour: 
%% 
%% - The Interface
%%
%%   The behaviour interface is a specific set of functions and associated 
%%   calling conventions. 
%%
%%   The gen_server behaviour interface contains six functions; 
%%   init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
%%   code_change/3.
%%
%%
%% - The Implementation
%%
%%   The application-specific code that the programmer pro- vides. A behaviour 
%%   implementation is a callback module that exports the functions required by 
%%   the interface. The implementation module should also contain an attribute
%%   -behaviour(...). that indicates the name of the behaviour it implements; 
%%   this allows the compiler to check that the module exports all the functions 
%%   of the interface. 
%%
%% 
%% - The Ccontainer
%% 
%%   This is a process that runs code from a library module and that uses 
%%   implementation callback modules to handle application-specific things. 
%%   (Technically, the container could consist of multiple processes working 
%%   closely together, but usually there is only one process.) 
%% 
%%   The important thing to keep in mind is that when your callback code is running, 
%%   it’s executed by a container, which is a process with identity and state 
%%   (including its mail-box). This is a lot like objects in object-oriented 
%%   programming, but with the addition that all these containers are living things 
%%   that are running code in parallel.
%% 

%% ----------------------------------------------------------------------------
-module(gen_server_skel).

%% The name of the 'behaviour' implemented. Allow checking by the 
%% compiler.
-behaviour(gen_server).

%% Export the standard methods required by the 'gen-server' behaviour.
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).


%% State (Tuple)
%%
%% A record defining the encapsulted state of the 'gen server'.
%% it is initialised as an empty tuple.
%%
-record(state, {}).


%% ----------------------------------------------------------------------------

%% Init
%%
%% Handle a 'asynchronous' message call to initialise the server.
%%
init([]) ->
    {ok, #state{}}.


%% Handle Call
%%
%% Handle a 'synchronous' message call to perform a function and 
%% return a result.
%%
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.


%% Handle Cast
%%
%% Handle a 'asynchronous' message call to perform a function.
%%
handle_cast(_Msg, State) ->
    {noreply, State}.


%% Handler Info
%%
%% Handles messages sent to a gen_server container that were not 
%% sent using one of the call or cast functions. 
%%
%% This is for out-of-band messages.
%%
%% -----
%% 
%% This callback is an important special case. It’s called to handle 
%% any messages that arrive in the mailbox of a gen_server that 
%% weren’t sent using one of the call or cast library functions 
%% (typically, naked messages sent with the plain old ! operator). 
%% There can be various reasons for such messages to find their way 
%% to the mailbox of a gen_server container—for example, that the 
%% callback code requested some data from a third party. 
%% 
%% In the case of your RPC server, you’ll receive data over TCP, 
%% which will be pulled off the socket and sent to your server 
%% process as plain messages.
%%
handle_info(_Info, State) ->
    {noreply, State}.


%% Terminate 
%%
%% Handle a 'asynchronous' message call to terminated the server.
%%
terminate(_Reason, _State) ->
    ok.


%% Code Change
%%
%%
%%
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
