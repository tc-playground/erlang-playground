%%%----------------------------------------------------------------------------
%%% @author Temple
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 14. Sep 2014 17:46
%%%----------------------------------------------------------------------------
-module(walks).
-author("Temple").


%%%============================================================================
%% Public API
%%%============================================================================

-export([
	plan_route/2
]).


%% Means that if the function plan_route/2 is called with two input arguments,
%% both of type point(), then it will return an object of type route().
%%
-spec plan_point_route(From::point(), To::point()) -> route().

%% Introduces a new type called direction() whose value is one of the atoms
%% north, south, east, or west.
%%
-type direction() :: north | south | east | west.

%% Means that the type point() is a tuple of two integers (integer() is a
%% predefined type).
%%
-type point() :: {P1::integer(), P2::integer()}.

%% Defines the type route() to be a list of 3-tuples, where each tuple contains
%% the atom go, an object of type direction, and an integer. The notation [X]
%% means a list of type X.
%%
-type route() :: [{go,direction(),integer()}].


-spec plan_vector_route(From::point(), To::point()) -> position().

-type angle() :: {Degrees::0..360, Minutes::0..60, Seconds::0..60}.

-type position() :: {latitude | longitude, angle()}.
