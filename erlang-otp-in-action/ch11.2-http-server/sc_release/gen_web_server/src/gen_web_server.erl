%%% BEHAVIOURS
%%%
%%% Behaviours have three parts: the container, the interface, and the 
%%% implementation. 
%%% 
%%% With existing behaviours, only a new implementation needs to be ccreated. 
%%% 
%%% To create a new *behaviour* the 'interface' and the 'container' must be 
%%% created. A new behaviour can then have potentially many implementations.
%%% 
%%% The container is the main chunk of reusable  code, which the implementation 
%%% modules will hook into via the callback functions defined by the interface. 
%%% 
%%% In this case, the container will be a generic web server.
%%%

%%%============================================================================
%%% @doc 
%%% Each instance of the gen_web_server behaviour consists of a arying and 
%%% unbounded number of processes. (For comparison, a gen_server instance only 
%%% consists of a single process.) 
%%%
%%% Each instance is distinct in that it manages a specific IP address/TCP port 
%%% combination. Each instance can handle a large number of incoming 
%%% connections on its particular port, starting a new separate 'gws_server' 
%%% process for each new connection.
%%% @end
%%%============================================================================
-module(gen_web_server).


%%%============================================================================
%%% GenWebServer - Init API Specification
%%%============================================================================

-export([
  start_link/3, 
  start_link/4
]).

%%%============================================================================
%%% GenWebServer - HTTP Callback API Specification
%%%============================================================================

-export([
  http_reply/1, 
  http_reply/2, 
  http_reply/3
]).


%%%============================================================================
%%% GenWebServer - Behaviour API Specification
%%%============================================================================

-export([behaviour_info/1]).

%% When the compiler sees a '–behaviour(x)'' declaration, it tries to call the 
%% module named x in order to find out what the interface should look like. The 
%% the compiler calls gen_server:behaviour_info(callbacks) to get the list of 
%% callback functions that a gen_server implementation module should export.
%%
%% NB: The atom 'callbacks' is currently the only defined parameter. For callbacks, 
%%     a list of function name/arity pairs, naming the callback functions that 
%%     the interface requires should be returned.
%%
%% This information makes it possible for the compiler to warn you if you say 
%% that your module is implementing a particular behaviour but you’ve forgotten 
%% to implement (or export) some of the expected callbacks. When you define a 
%% new behaviour, you should provide this function.
%%
%% This 'behviour' defines an 'HTTP Service'. Implementing services must handle 
%% these callbacks.
%%
behaviour_info(callbacks) -> [
  {init,1},                 % The init/1 callback initializes each new gws_server 
                            % connection handler rather than just once for the 
                            % entire behaviour instance.
  {head, 3},                % Handle 'HEAD' request.
  {get, 3},                 % Handle 'GET' request.
  {delete, 3},              % Handle 'DELETE' request.
  {options, 4},             % Handle 'OPTIONS' request.
  {post, 4},                % Handle 'POST' request.
  {put, 4},                 % Handle 'PUT' request.
  {trace, 4},               % Handle 'TRACE' request.
  {other_methods, 4}        % Handles HTTP methods other than the most common ones;
                            % it could also be used to implement HTTP extensions 
                            % like WebDAV.
  ];
%% NB: The atom 'callbacks' is currently the only defined parameter. Undefined.
behaviour_info(_Other) ->
  undefined.



%%%============================================================================
%%% GenWebServer - Init API Implementation
%%%============================================================================

%% Start a new instances of the behaviour using the default IP address for the 
%% machine.
%%
%% Any additional arguments that will be passed on to the init/1 callback 
%% function for each new connection handler.
%%
start_link(Callback, Port, UserArgs) ->
  start_link(Callback, undefined, Port, UserArgs).

%% Start a new instances of the behaviour using the specified IP address for the 
%% machine (for a machine that has multiple network interfaces).
%%
%% Any additional arguments that will be passed on to the init/1 callback 
%% function for each new connection handler. 
%%
start_link(Callback, IP, Port, UserArgs) ->
    gws_connection_sup:start_link(Callback, IP, Port, UserArgs).



%%%============================================================================
%%% GenWebServer - HTTP Callback API Specification
%%%============================================================================

% Utility function to make it easy for implementation modules of gen_web_server 
%% to create proper HTTP replies.
%%
http_reply(Code, Headers, Body) ->
  ContentBytes = iolist_to_binary(Body),
  Length = byte_size(ContentBytes),
  [io_lib:format("HTTP/1.1 ~s\r\n~sContent-Length: ~w\r\n\r\n",
  [response(Code), headers(Headers), Length]), ContentBytes].

% Utility function to make it easy for implementation modules of gen_web_server 
%% to create proper HTTP replies.
%%
http_reply(Code) ->
    http_reply(Code, <<>>).

% Utility function to make it easy for implementation modules of gen_web_server 
%% to create proper HTTP replies.
%%
http_reply(Code, Body) ->
    http_reply(Code, [{"Content-Type", "text/html"}], Body).



%%%============================================================================
%%% Private Functions
%%%============================================================================

%% Create Response Headers
%%
headers([{Header, Text} | Hs]) ->
    [io_lib:format("~s: ~s\r\n", [Header, Text]) | headers(Hs)];
headers([]) ->
    [].

%% Get Response HEader String from Codes.
%%
%% Fill in the missing status codes below if you want...
%%
response(100) -> "100 Continue";
response(200) -> "200 OK";
response(404) -> "404 Not Found";
response(501) -> "501 Not Implemented";
response(Code) -> integer_to_list(Code).
