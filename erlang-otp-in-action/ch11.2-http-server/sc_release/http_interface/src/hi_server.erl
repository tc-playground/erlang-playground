%%%============================================================================
%%% @doc 
%%% The hi_server module is an implementation of the 'gen_web_server' behaviour.
%%%
%%% Much as in a gen_server implementation module, you have the behaviour 
%%% declaration, the export list for the callbacks, and some API functions for 
%%% things like starting a server instance. In this case, the start_link 
%%% functions want at least a port number as input, to be passed from hi_sup. 
%%% Also note that an empty list is given to gen_web_server:start_link() as the 
%%% UserArgs argument; this particular server implementation doesn’t use it for 
%%% anything.
%%%
%%% The init/1 callback function is called by the gen_web_server for each new 
%%% connection, to initialize the user_data field. The UserArgs argument that 
%%% was given to gen_web_server:start_link() is passed unchanged to init/1, so 
%%% in this particular implementation you expect an empty list as input. But 
%%% this simple server doesn’t use the user_data feature of gen_web_server for 
%%% anything, so init/1 returns another empty list here.
%%%
%%% @end
-module(hi_server).


%%%============================================================================
%%% Public API
%%%============================================================================

-export([
  start_link/1, 
  start_link/2]
).


%%%============================================================================
%%% OTP GenWebServer Behaviour
%%%============================================================================

-behaviour(gen_web_server).


%% gen_web_server callbacks
-export([
  init/1, 
  get/3, 
  delete/3, 
  put/4, 
  post/4,
  head/3, 
  options/4, 
  trace/4, 
  other_methods/4
]).


%%%============================================================================
%%% Public API Implementation
%%%============================================================================

start_link(Port) ->
  gen_web_server:start_link(?MODULE, Port, []).

start_link(IP, Port) ->
  gen_web_server:start_link(?MODULE, IP, Port, []).


%%%============================================================================
%%% OTP WebGenServer Callback Implementation
%%%============================================================================

init([]) ->
  {ok, []}.


%% Simple Implementation
%%
%% Binary pattern matching to strip the leading slash from the key (which is 
%% given as part of the URI). To keep this example simple, the keys and the 
%% stored data are binaries representing plain text. Other Erlang terms that may 
%% be stored in the cache aren’t handled; in particular, if simple_cache:lookup(Key) 
%% succeeds, the found value is expected to be a binary, string, or IO-list, or 
%% the server can’t send it back on the socket as it is.
%%


get(
    {http_request, 'GET', {abs_path, <<"/",Key/bytes>>}, _},
    _Head, 
    _UserData
    ) ->
  case simple_cache:lookup(Key) of
    {ok, Value} ->
        gen_web_server:http_reply(200, [], Value);
    {error, not_found} ->
        gen_web_server:http_reply(404, "Sorry, no such key.")
  end.


delete(
    {http_request, 'DELETE', {abs_path, <<"/",Key/bytes>>}, _}, 
    _Head, 
    _UserData) ->
  simple_cache:delete(Key),
  gen_web_server:http_reply(200).



put(
    {http_request, 'PUT', {abs_path, <<"/",Key/bytes>>}, _},
    _Head, 
    Body, 
    _UserData
    ) ->
    simple_cache:insert(Key, Body),
    gen_web_server:http_reply(200).


% Not applicable for this rest service.
%
post(
    _Request, 
    _Head, 
    _Body, 
    _UserData
    ) ->
  gen_web_server:http_reply(501).

% Not applicable for this rest service.
%
head(
    _Request, 
    _Head, 
    _UserData
    ) ->
  gen_web_server:http_reply(501).

% Not applicable for this rest service.
%
options(
    _Request, 
    _Head, 
    _Body, 
    _UserData
    ) ->
  gen_web_server:http_reply(501).

% Not applicable for this rest service.
%
trace(
    _Request, 
    _Head, 
    _Body, 
    _UserData
    ) ->
  gen_web_server:http_reply(501).

% Not applicable for this rest service.
%
other_methods(
    _Request, 
    _Head, 
    _Body, 
    _UserData
    ) ->
  gen_web_server:http_reply(501).




