%%%----------------------------------------------------------------------------
%%% @author Temple
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 12. Sep 2014 07:09
%%%----------------------------------------------------------------------------
-module(sws).


%%%============================================================================
%% Public API
%%%============================================================================

-export([
	start/0,
	start/1
]).

% Cowboy Callbacks
-export([
	init/3,       % Initialise a new conection.
	handle/2,     % Handle a request.
	terminate/3   % Clean up after a request has been processed.
]).


%%%============================================================================
%% Public API Implementation
%%%============================================================================

%%-----------------------------------------------------------------------------
%% @doc
%%
%% @end
%%-----------------------------------------------------------------------------
start() ->
	start(8877).

%%-----------------------------------------------------------------------------
%% @doc
%%
%% @end
%%-----------------------------------------------------------------------------
start(Port) ->

	io:format("Starting Cowboy on port ~p...~n", [Port]),

	ok = application:start(crypto),
	ok = application:start(ranch),
	ok = application:start(cowlib),
	ok = application:start(cowboy),

	N_acceptors = 10,

	Dispatch = cowboy_router:compile([
		%% {URIHost, list({URIPath, Handler, Opts})}
		{'_', [{'_', sws, []}]}
		]),

	cowboy:start_http(
		my_sws,
		N_acceptors,
		[{port, Port}],
		[{env, [{dispatch, Dispatch}]}]
	).


%%%============================================================================
%%% Callback API Implementation
%%%============================================================================

%%% The module names given in the dispatcher patterns must provide three
%%% callback routines: init/3, handle/3, and terminate/2.


%%-----------------------------------------------------------------------------
%% @doc
%% Handles a new connection.
%%
%% Params:
%% {tcp, http}: An HTTP connection.
%% Req        : The 'request object' context.
%% Opts       : Passed in paramters.
%%
%%
%% init/3 conventionally returns the tuple {ok, Req, State}, which causes the
%% web server to accept the connection.
%%
%% Req is the request object, and State is a private state associated with the
%% connection. If the connection was accepted, then the HTTP driver will call
%% the function handle/2 with the request object and state that was returned
%% from the init function.
%%
%% Returns
%%
%% {ok, Req, State},
%%

%% @end
%%-----------------------------------------------------------------------------
init({tcp, http}, Req, _Opts) ->
	{ok, Req, undefined}.


%%-----------------------------------------------------------------------------
%% @doc
%%
%% @end
%%-----------------------------------------------------------------------------
handle(Req, State) ->
	% Extract the 'path' to the resource as a binary.
	{Path, Req1} = cowboy_req:path(Req),
	% Read the response as a <<binary>>.
	Response = read_file(Path),
	% Reply
	{ok, Req2} = cowboy_req:reply(200, [], Response, Req1),
	{ok, Req2, State}.


%%-----------------------------------------------------------------------------
%% @doc
%%
%% @end
%%-----------------------------------------------------------------------------
terminate(_Reason, _Req, _State) ->
	ok.


%%%============================================================================
%%% Private
%%%============================================================================

%%-----------------------------------------------------------------------------
%% @doc
%%
%% @end
%%-----------------------------------------------------------------------------
read_file(Path) ->
	File = ["."|binary_to_list(Path)],
	case file:read_file(File) of
		{ok, Bin} ->
			Bin;
		_ ->
			["<pre>cannot read:", File, "</pre>"]
	end.




