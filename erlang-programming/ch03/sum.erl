-module(sum).

-export([sum/1]).
-export([sum/2]).

% Write a function sum/1 which, given a positive integer N, will return 
% the sum of all the integers between 1 and N.
%
% Example:
%   sum(5) ⇒ 15.
%
sum(0) -> 0;
sum(IntVal) ->
	IntVal + sum(IntVal - 1).


% Write a function sum/2 which, given two integers N and M, where N =< M, 
% will return the sum of the interval between N and M. If N > M, you want 
% your process to terminate abnormally.
%
% Example:
% 	sum(1,3) ⇒ 6. sum(6,6) ⇒ 6.
%
sum(IntVal1, IntVal2) when IntVal1 == IntVal2 ->
	IntVal2;
sum(IntVal1, IntVal2) when IntVal1 < IntVal2 ->
	IntVal1 + sum(IntVal1 + 1, IntVal2);
sum(IntVal1, IntVal2) when IntVal1 > IntVal2 ->
	{error, "Bad input values: IntVal1 > IntVal2.", {IntVal1, IntVal2}}.
