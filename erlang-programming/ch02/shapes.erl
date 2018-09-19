-module(shapes).

-export([area/1]).

% Create a new module called shapes and copy the area function in it. 
% Do not forget to include all the module and export directives. 
% Compile it and run the area function from the shell. When you compile 
% it, why do you get a warning that variable Other is unused? What happens 
% if you rename the variable to _Other?
%
area({square, Side}) ->
	Side * Side;
area({circle, Radius}) -> 
	math:pi() * Radius * Radius;
area({triangle, A, B, C}) ->
	S = (A + B + C)/2, 
	math:sqrt(S*(S-A)*(S-B)*(S-C));
% area(Other) ->
% 	{error, invalid_object}.
area(_) ->
	{error, invalid_object}.