%%%-------------------------------------------------------------------
%%% @author Temple
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. Sep 2014 19:11
%%%-------------------------------------------------------------------

-module(hello).
-export([start/0]).

start() ->
  io:format("Hello world~n").

% Run from Erlang Shell
%
% $ erl
% Erlang R16B ... 1> c(hello). {ok,hello}
% 2> hello:start(). Hello world
% ok
% 3> halt().
% $


% Run from Command Line
%
% $ erlc hello.erl
% $ erl -noshell -s hello start -s init stop Hello world
