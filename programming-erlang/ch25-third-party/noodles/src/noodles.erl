%%%----------------------------------------------------------------------------
%%% @author Temple
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 12. Sep 2014 07:09
%%%----------------------------------------------------------------------------
-module(noodles).
-author("Temple").


%%%============================================================================
%% Public API
%%%============================================================================

-export([
	start/0
]).


%%%============================================================================
%% Public API Implementation
%%%============================================================================


start() ->
	io:format("I am just a litle the Noodles!~n"),
	Handle = bitcask:open("noodles_db", [read_write]),
	N = fetch(Handle),
	store(Handle, N+1),
	io:format("Noodles has been started ~p times~n",[N]),
	bitcask:close(Handle),
	init:stop().


store(Handle, N) ->
	bitcask:put(Handle, <<"noodle_executions">>, term_to_binary(N)).


fetch(Handle) ->
	case bitcask:get(Handle, <<"noodle_executions">>) of
		not_found -> 1;
		{ok, Bin} -> binary_to_term(Bin)
	end.