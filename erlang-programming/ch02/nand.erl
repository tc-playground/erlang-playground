% Write a module boolean.erl that takes logical expressions and Boolean values 
% (repre- sented as the atoms true and false) and returns their Boolean result. 
% The functions you write should include b_not/1, b_and/2, b_or/2, and b_nand/2. 
% You should not use the logical constructs and, or, and not, but instead use 
% pattern matching to achieve your goal.
%
% Test your module from the shell. Some examples of calling the exported 
% functions in your module include:
% 	bool:b_not(false) ⇒ true
% 	bool:b_and(false, true) ⇒ false 
%   bool:b_and(bool:b_not(bool:b_and(true, false)), true) ⇒ true
%

-module(nand).
-export([b_nand/2]).
-export([b_not/1]).
-export([b_and/2]).
-export([b_or/2]).

b_nand(true, true) -> false;
b_nand(true, false) -> true;
b_nand(false, true) -> true;
b_nand(false, false) -> true.

b_not(Expr1) -> b_nand(Expr1, Expr1).

b_and(Expr1, Expr2) -> b_not(b_nand(Expr1, Expr2)).
% b_and(Expr1, Expr2) -> b_nand(b_nand(Expr1, Expr2), b_nand(Expr1, Expr2)).

b_or(Expr1, Expr2) -> b_nand(b_nand(Expr1, Expr1), b_nand(Expr2, Expr2)).