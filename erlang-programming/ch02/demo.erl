% A simple demo module
-module(demo). 

-export([double/1]).

-company({name, "Ripoto"}).
-author("Tim Langford").
-email("tim.langford@gmail.com").
-date("2014-05-04").

% Public Functions
double(Value) -> times(Value, 2).

%Private functions.
times(X,Y) -> X*Y.