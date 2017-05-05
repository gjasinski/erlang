-module(power).

-export([pow/2]).

pow(Base,1) ->
	Base;

pow(Base,Power) ->
	Base*pow(Base,Power-1).