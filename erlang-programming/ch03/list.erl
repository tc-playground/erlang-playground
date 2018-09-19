-module(list).

-export([create/1]).
-export([rcreate/1]).

-define(MAXSIZE, 10000).


% Write a function that returns a list of the format [1,2,..,N-1,N]. 
% Example:
% 	create(3) ⇒ [1,2,3].
%
create(0) ->
	{error};
create(1) ->
	[1];
create(IntVal) when IntVal =< ?MAXSIZE ->
	create(IntVal - 1) ++ [IntVal];
create(IntVal) when IntVal > ?MAXSIZE ->
	{error, "Input IntVal too  large!", IntVal}.


% Write a function that returns a list of the format [N, N-1,..,2,1].
% Example:
% 	reverse_create(3) ⇒ [3,2,1].
%
rcreate(0) ->
	{error};
rcreate(1) ->
	[1];
rcreate(IntVal) when IntVal =< ?MAXSIZE ->
	[IntVal|rcreate(IntVal - 1)];
rcreate(IntVal) when IntVal > ?MAXSIZE ->
	{error, "Input IntVal too  large!", IntVal}.