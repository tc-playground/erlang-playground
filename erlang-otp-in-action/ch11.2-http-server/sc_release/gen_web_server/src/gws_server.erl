%%% Uses the 'zero timeout init trick' of returning a zero timeout from the 
%%% 'init/1' callback in order to defer initialization to 
%%% 'handle_info(timeout, State)', which blocks on gen_tcp:accept() and then 
%%% asks the supervisor to spawn another handler proces.

%%%============================================================================
%%% @doc 
%%% 
%%% @end
%%%============================================================================
-module(gws_server).


%%%============================================================================
%%% Public API
%%%============================================================================

%% API
-export([start_link/3]).


%%%============================================================================
%%% OTP GenServer Behaviour
%%%============================================================================

-behaviour(gen_server).

-export([
  init/1, 
  handle_call/3, 
  handle_cast/2, 
  handle_info/2,
  terminate/2, 
  code_change/3
]).


%%%============================================================================
%%% Private State
%%%============================================================================

-record(
  state, {
    lsock,                  % New client listener socket
    socket,                 % This client socket
    request_line,           % HTTP request line / url.
    headers = [],           % HTTP Headers.
    body = <<>>,            % HTTP Body (binary).
    content_remaining = 0,  % HTTP Content not yet recieved.
    callback,               % The name of the behaviour implementation module.
    user_data,              % Application data passed to the callback module.
    parent                  % Process ID of the gws_connection_sup supervisor
  }
).


%%%============================================================================
%%% Public API Implementation
%%%============================================================================

%% There can be many parallel instances of 'gen_web_server', so the gws_server 
%% processes need to be told who their particular supervisor is. 
%% 
%% This is set up in the start_link/3 function, where the pid of the caller 
%% (assumed to always be the gws_connection_sup process) is automatically passed 
%% as an argument to gws_connection_sup:init/1.
%%
%% The start_link/3 function also takes a behaviour callback module, a listening 
%% socket, and some extra arguments from the user of the gen_web_server behaviour, 
%% all of which is passed on to init/1. There, you see the first interaction 
%% with the behaviour callback module provided by the user: the UserArgs argument 
%% is passed to the init/1 function of the callback module (just like a 
%% gen_server calls your init/1 function when it starts a new instance). This 
%% returns a UserData value, which is stored in the state record along with the 
%% other startup arguments.
%% 
start_link(Callback, LSock, UserArgs) ->
  gen_server:start_link(?MODULE,[Callback, LSock, UserArgs, self()], []).


%%%============================================================================
%%% OTP GenServer Callbacks
%%%============================================================================

init([Callback, LSock, UserArgs, Parent]) ->
  {ok, UserData} = Callback:init(UserArgs),
  State = #state{
    lsock = LSock, 
    callback = Callback, 
    user_data = UserData, 
    parent = Parent
  },
  {ok, State, 0}. % 'zero timeout init trick': 
                  % Induces handle_info/2(timeout, State)


handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast(_Request, State) ->
  {noreply, State}.

%%-----------------------------------------------------------------------------
%% @doc handle_info/2 
%% The TCP socket configured with '{packet, http_bin}'' option. 
%% The socket auto-parses http input and sends it as erlang tagged tuples.
%% @end
%%-----------------------------------------------------------------------------
%%
%% Handle the 'http' event tuple where the data tuple is tagged 'http_request'.
%%
handle_info(
    {http, _Sock, {http_request, _, _, _}=Request}, 
    State
    ) ->
  inet:setopts(State#state.socket, [{active,once}]),   % Socket Read. Reset.
  % Store the Request in State#request_line.
  {noreply, State#state{request_line = Request}};
%% Handle the 'http' event tuple where the data tuple is tagged 'http_header'.
%%
handle_info(
    {http, _Sock, {http_header, _, Name, _, Value}},
    State
    ) ->
  inet:setopts(State#state.socket, [{active,once}]),  % Socket Read. Reset.
  % Build the header and add it to the state.
  {noreply, header(Name, Value, State)};
%% Handle the 'http' event tuple where the data tuple is tagged 'http_eoh'.
%% there is no 'content_remaining'.
%%
%% NB: When the empty line that signals the end of the headers is reached, the 
%% socket sends a message in the following form:
%% 
%%  {http, _Socket, http_eoh}
%%
%% As there is no body to process; handle the request.
%%
handle_info(
    {http, _Sock, http_eoh}, 
    #state{content_remaining = 0} = State
    ) ->
  % The headers have been all read!
  % There is no binary data to read!
  % The whole HTTP request has been obtained! Process the request!
  {stop, normal, handle_http_request(State)};
%% Handle the 'http' event tuple where the data tuple is tagged 'http_eoh'.
%% there is 'content_remaining'.
%%
%% NB: When the empty line that signals the end of the headers is reached, the 
%% socket sends a message in the following form:
%% 
%%  {http, _Socket, http_eoh}
%%
%% As there is data to be read switch the socket from {packet, http_bin} to 
%% {packet, raw}, telling it to stop parsing HTTP-specific data.
%%
handle_info(
    {http, _Sock, http_eoh}, 
    State
    ) ->
  % The headers have been all read!
  % Now read any binary data in the request body...
  % Socket Read. Reset. Set to reading raw binary...
  inet:setopts(State#state.socket, [{active,once}, {packet, raw}]), 
  {noreply, State};
%% Handle the 'tcp' event tuple where the Data is binary.
%%
handle_info(
    {tcp, _Sock, Data}, 
    State
    ) when is_binary(Data) ->
  % Calculate the data remaining and store it in the State.
  ContentRem = State#state.content_remaining - byte_size(Data),
  % Append the data to the current Data and store it in the State.
  Body = list_to_binary([State#state.body, Data]),
  NewState = State#state{body = Body, content_remaining = ContentRem},
  if 
    ContentRem > 0 ->
      % If there is still content to recieve; reset the socketfor the next chunk.
      inet:setopts(State#state.socket, [{active,once}]),   % Socket Read. Reset.
      {noreply, NewState};
    true ->
      % The headers have been all read!
      % The binary data body has been read!
      % The whole HTTP request has been obtained! Process the request!
      {stop, normal, handle_http_request(NewState)}
  end;
%% Handle the 'tcp_closed' event tuple.
%%
handle_info(
  {tcp_closed, _Sock}, 
  State
  ) ->
  {stop, normal, State};
%% 'zero timeout init trick
%% Starts listening on socket in this process and creates a new 
%% 'gws_connection_sup' to accept new connections.
%%
%% SOCKET OPTIONS
%%
%% Socket Option: {active, true}
%%
%% Incoming data is automatically read from the socket as soon as it’s ready 
%% and is then sent as an Erlang  message to the process that owns the socket. 
%% This allows a nice  event-based style, but the disadvantage is that an active 
%% socket has  no flow control. A client that transmits a lot of data very fast 
%% could make the server use up all of its memory just for the message queue.
%%
%% 
%% Socket Option: {active, false}
%% 
%% The receiving process must read the data off the socket explicitly with a 
%% gen_tcp:read() whenever it’s ready. This makes the built-in flow control in 
%% TCP block the sender while the receiver isn’t reading, fixing the 
%% out-of-memory vulnerability. The problem is that it doesn’t make for a very 
%% Erlang-like programming style.
%%
%%
%% Socket Option: {active, false}
%% 
%% Puts the socket in 'active mode' until some data is received, causing an 
%% Erlang message to be sent to the owning process, and then automatically  
%% sets it back to 'passive mode'. This then enables the flow control so that no  
%% further data is read and no messages are sent until the controlling process 
%% indicates that it’s ready. Usually, it does this by setting the socket back 
%% to {active, once} and going back to waiting for the next message.
%%
%% For example:
%%
%% start() ->
%%   {ok, LSock} = gen_tcp:listen(1055, [binary, {active, false}]),
%%   {ok, Socket} = gen_tcp:accept(LSock),
%%   loop(Socket).
%% 
%% loop(Socket) ->
%%   inet:setopts(Socket, [{active,once}]),
%%   receive
%%     {tcp, Socket, Data} ->
%%       io:format("got ~p~n", [Data]),
%%       loop(Socket);
%%     {tcp_closed, _Socket} ->
%%       ok
%%   end.
%%
%% Handle the 'timeout' event tuple.
%%
handle_info(
  timeout, 
  #state{lsock = LSock, parent = Parent} = State
  ) ->
  % Listen on the specified socket.
  {ok, Socket} = gen_tcp:accept(LSock),
  % Start a new 'gws_connection_sup' process.
  gws_connection_sup:start_child(Parent),
  % set the 'active one' option on the socket
  inet:setopts(Socket,[{active,once}]),
  {noreply, State#state{socket = Socket}}.


terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.


%%%============================================================================
%%% Private Functions
%%%============================================================================

%% Handle the 'Content-Length' (atom) header.
%%
%% Determine the 'Content-Length' and update the State.
%%
header('Content-Length' = Name, Value, State) ->
  % Determine the expected 'Content-Length'.
  ContentLength = list_to_integer(binary_to_list(Value)),
  % Store the expected 'Content-Length'. This will be decremented as data is 
  % recieved.
  State#state{content_remaining = ContentLength,
  % Store the header in the State.
  headers = [{Name, Value} | State#state.headers]};
%% Handle the 'Expect 100-continue' header value pair.
%%
%% Return a 100 continue to the client and add the header to the state.
%%
header(<<"Expect">> = Name, <<"100-continue">> = Value, State) ->
  % Send 100 response to client. If we don’t send this reply, the client will 
  % pause a couple of seconds before transmitting the body, which slows things 
  % down quite a bit.
  gen_tcp:send(State#state.socket, gen_web_server:http_reply(100)),
  % Update header state.
  State#state{headers = [{Name, Value} | State#state.headers]};
%% Add the specified header to the State.
%%
header(Name, Value, State) ->
  State#state{headers = [{Name, Value} | State#state.headers]}.

%% Handle the response!
%%
%% The headers have been all read!
%% There is no binary data to read!
%% The whole HTTP request has been obtained! Process the request!
%%
%% This is finally where the main interaction happens between the 
%% 'gen_web_server container code' and the 'user-supplied behaviour implementation'
%% (the callback module).
%%
handle_http_request(
  #state{
    callback = Callback,
    request_line = Request,
    headers = Headers,
    body = Body,
    user_data = UserData
    } = State) ->
  
  {http_request, Method, _, _} = Request,
  
  % Dispatch the request to the Callback module (the provided 'gen_web_server' 
  % implementation) to handle the request and obtain a result.
  Reply = dispatch(Method, Request, Headers, Body, Callback, UserData),
  % Send the response to the client.
  gen_tcp:send(State#state.socket, Reply),
  % Return the State.
  State.

%% Dispatchio!
%%
dispatch('GET', Request, Headers, _Body, Callback, UserData) ->
  Callback:get(Request, Headers, UserData);
dispatch('DELETE', Request, Headers, _Body, Callback, UserData) ->
  Callback:delete(Request, Headers, UserData);
dispatch('HEAD', Request, Headers, _Body, Callback, UserData) ->
  Callback:head(Request, Headers, UserData);
dispatch('POST', Request, Headers, Body, Callback, UserData) ->
  Callback:post(Request, Headers, Body, UserData);
dispatch('PUT', Request, Headers, Body, Callback, UserData) ->
  Callback:put(Request, Headers, Body, UserData);
dispatch('TRACE', Request, Headers, Body, Callback, UserData) ->
  Callback:trace(Request, Headers, Body, UserData);
dispatch('OPTIONS', Request, Headers, Body, Callback, UserData) ->
  Callback:options(Request, Headers, Body, UserData);
dispatch(_Other, Request, Headers, Body, Callback, UserData) ->
  Callback:other_methods(Request, Headers, Body, UserData).






