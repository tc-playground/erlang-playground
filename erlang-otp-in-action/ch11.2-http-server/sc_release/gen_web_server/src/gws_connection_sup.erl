%%%============================================================================
%%% @doc 
%%% The supervisor implemented by the gws_connection_sup module will be a 
%%% 'simple-one-for-one' supervisor, used as a factory for handler processes.
%%% @end
%%%============================================================================
-module(gws_connection_sup).

%%% Any crashes that occur in the gws_connection_sup group will have no effect 
%%% on the separate worker process (other than what the supervisor decides). 
%%% Because of this principle, you should generally:
%%% 
%%% !!! KEEP SUPERVISOR FREE FROM APPLICATION SPECIFIC CODE !!!
%%%

%%%============================================================================
%%% Public Init API Interface
%%%============================================================================

-export([start_link/4, start_child/1]).


%%%============================================================================
%%% OTP Supervisor Behaviour
%%%============================================================================

-behaviour(supervisor).

-export([init/1]).



%%%============================================================================
%%% Public Init API Implementation
%%%============================================================================

%% Start the 'gen_connection_sup' instance and also kicks off the first 
%% gws_server child process as soon as the supervisor is up and running. 
%% 
%% We could also place the responsibility for doing this in 
%% gen_web_server:start_link/4, but this way it’s guaranteed that a new 
%% 'gws_connection_sup process' will always have a process that is listening 
%% on the socket, ready to handle any incoming connection.
%%
start_link(Callback, IP, Port, UserArgs) ->
  % Use the superviser module to start a new 'gws_connection_sup' process.
  % NB: As this is a library, not, an application...
  {ok, Pid} = supervisor:start_link(?MODULE, [Callback, IP, Port, UserArgs]),
  % User the new superviser to start an initial 'gws_server' chld process.
  start_child(Pid),
  {ok, Pid}.


%% Start a new 'gws_server' child process. 
%% 
start_child(Server) ->
  supervisor:start_child(Server, []).



%%%============================================================================
%%% OTP Supervisor Behaviour Implementation
%%%============================================================================ 

%%% TCP flow control and active/passive sockets
%%%
%%% Although active mode is cleaner and has more of an Erlang/OTP feel to it, 
%%% it doesn’t provide any flow control. In active mode, the Erlang runtime 
%%% system reads data from the socket as quickly as it can and passes it on as 
%%% Erlang messages to the socket’s owner process. If a client is able to send 
%%% data faster than the receiver can read it, it causes the message queue to 
%%% grow until all available memory is used up. In passive mode, the owner 
%%% process must explicitly read data off the socket, adding complexity to the 
%%% code but providing more control over when data enters the system and at 
%%% what rate; the built-in flow control in TCP blocks the sender 
%%% automatically.
%%%

init([Callback, IP, Port, UserArgs]) ->

  % Define the BasicSockOpts.
  %
  % NB: Without {reuseaddr, true}, the server can’t be restarted (on the same 
  %     port) until the OS kernel has timed out on the listening socket.
  %
  BasicSockOpts = [
    binary,             % Incoming data is delivered as binaries (not strings).
    {active, false},    % The socket is opened in passive mode.
    {packet, http_bin}, % Incoming data is expected to be formatted as HTTP.
    {reuseaddr, true}   % Allows local port numbers to be reused (sooner).
    ],
  
  % If an IP is defined then add it to the BasicSockOpts.
  %
  SockOpts = 
    case IP of
      undefined -> BasicSockOpts;
      _         -> [{ip,IP} | BasicSockOpts]
    end,
    
  % Open a new listening Socket.
  %
  {ok, LSock} = gen_tcp:listen(Port, SockOpts),

  % Define the 'gws_connection_sup' init process.
  %
  Server = {             % The Id to identify the child specification.
    gws_server, {        % The apply(M, F, A) tuple to start the process.
    gws_server, 
      start_link, 
      [Callback, LSock, UserArgs]
    },
    temporary,           % Child process always restarted.
    brutal_kill,         % Terminate child: immediately
    worker,              % Child is a worker process.
    [gws_server]         % The name of the callback module.
    },

    RestartStrategy = {simple_one_for_one, 1000, 3600},
    
    {ok, {RestartStrategy, [Server]}}.








