%%%----------------------------------------------------------------------------
%%% @author Temple
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 09. Sep 2014 19:11
%%%----------------------------------------------------------------------------
-module(afile_server).
-author("Temple").


%% Erlang Shell Interaction ---------------------------------------------------
%%
%% 1> c(afile_server).
%% {ok,afile_server}
%% 2> FileServer = afile_server:start("."). <0.47.0>
%% 3> FileServer ! {self(), list_dir}. {<0.31.0>,list_dir}
%% 4> receive X -> X end.
%% { < 0.47.0>,
%% {ok, ["file1.nfo", "file2.erl","file3.erl"]}}


%%%============================================================================
%%% Public API
%%%============================================================================

-export([
	start/1,
	loop/1
]).


%%%============================================================================
%%% Public API - Implementation
%%%============================================================================

start(Dir) -> spawn(afile_server, loop, [Dir]).


loop(Dir) ->

	io:format("Server ~p awaiting client request...~n", [self()]),

	receive

		{Client, list_dir} ->
			io:format("list_dir: ~p~n", [Dir]),
			Client ! {self(), file:list_dir(Dir)},
			io:format("Sent remote dir ls to client: ~p~n", [Client]);

		{Client, {get_file, FileName}} ->
			io:format("get_file: ~p~n", [FileName]),
			FullFilePath = filename:join(Dir, FileName),
			io:format("Target File: ~p~n", [FullFilePath]),
			Client ! {self(), file:read_file(FullFilePath)},
			io:format("Sent file contents to client: ~p~n", [Client]);

		{Client, {put_file, FileName, FileData}} when is_binary(FileData) ->
			io:format("write_file: ~p~n", [FileName]),
			FullFilePath = filename:join(Dir, FileName),
			io:format("Target File: ~p~n", [FullFilePath]),
			file:write_file(FullFilePath, FileData),
			io:format("Wrote File: ~p -> ~p ~n", [FullFilePath, FileData]),
			Client ! {self(), ok},
			io:format("Obtained file contents from client: ~p~n", [Client]);

		{Client, X} ->
			Client ! {self(), {error,  bad_input, X}}

	end,

	io:format("Server ~p handled client request~n", [self()]),

	loop(Dir).
