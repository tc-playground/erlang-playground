%%%----------------------------------------------------------------------------
%%% @author Temple
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 12. Sep 2014 07:09
%%%----------------------------------------------------------------------------
-module(sws_rpc).


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
		{'_', [{'_', sws_rpc, []}]}
		]),

	cowboy:start_http(
		my_sws,
		N_acceptors,
		[{port, Port}],
		[{env, [{dispatch, Dispatch}]}]
	).


start_from_shell([PortAsAtom]) ->
	PortAsInt = list_to_integer(atom_to_list(PortAsAtom)),
	start(PortAsInt).




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
	% io:format("init(Type:{tcp, http}, Req:~p, _Opts:~p)", [Req, _Opts]),
	{ok, Req, undefined}.


%%-----------------------------------------------------------------------------
%% @doc
%%
%% @end
%%-----------------------------------------------------------------------------
handle(Req, State) ->
	% io:format("handle(Req:~p, State:~p)", [Req, State]),
	{Path, Req1} = cowboy_req:path(Req),
	handle(Path, Req1, State).



%%-----------------------------------------------------------------------------
%% @doc
%%
%% @end
%%-----------------------------------------------------------------------------
terminate(_Reason, _Req, _State) ->
	% io:format(
	%	"terminate(_Reason:~p, _Req:~p, _State:~p)", [_Reason, _Req, _State]
	%	),
	ok.


%%%============================================================================
%%% Private
%%%============================================================================


%%-----------------------------------------------------------------------------
%% @doc
%%
%% @end
%%-----------------------------------------------------------------------------
handle(<<"/rpc">>, Req, State) ->
	% Parse RQ url...
	{Args, Req1} = cowboy_req:qs_vals(Req),
	% Parse RQ body...
	{ok, Bin, Req2} = cowboy_req:body(Req1),
	io:format("Trying to decode:~n~p~n", [Bin]),
	% Decode the JSON body to erlang terms...
	Val = mochijson2:decode(Bin),
	io:format("Decoded:~n~p~n", [Val]),
	% Execute the RPC...
	Response = do_rpc(Args, Val),
	% Convert the ErlangTerm result to JSON...
	Json = mochijson2:encode(Response),
	% Send the reply..
	{ok, Req3} = cowboy_req:reply(200, [], Json, Req2), {ok, Req3, State};
%%
handle(Path, Req, State) ->
	Response = read_file(Path),
	{ok, Req1} = cowboy_req:reply(200, [], Response, Req),
	{ok, Req1, State}.

%%-----------------------------------------------------------------------------
%% @doc
%%
%% @end
%%-----------------------------------------------------------------------------
do_rpc([{<<"mod">>,MB},{<<"func">>,FB}], X) ->
	Mod = list_to_atom(binary_to_list(MB)),
	Func = list_to_atom(binary_to_list(FB)),
	apply(Mod, Func, [X]).


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




