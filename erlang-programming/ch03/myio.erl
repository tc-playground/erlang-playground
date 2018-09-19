-module(myio).

-export([printnums/1]).
-export([printevennums/1]).

% Write a function that prints out the integers between 1 and N. 
% Hint: use io:format("Number:~p~n",[N]).
%
printnums(1) ->
	io:format("~B ", [1]);
printnums(IntVal) when IntVal > 0 -> 
	printnums(IntVal - 1),
	io:format("~B ", [IntVal]).


% Write a function that prints out the even integers between 1 and N. 
% Hint: use guards.
%
printevennums(2) ->
	io:format("~B ", [2]);
printevennums(IntVal) when IntVal > 0, IntVal rem 2 == 0 ->
	printevennums(IntVal - 2), 
	io:format("~B ", [IntVal]);
printevennums(IntVal) when IntVal > 0, IntVal rem 2 /= 0 -> 
	printevennums(IntVal - 1).