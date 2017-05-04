%%%-------------------------------------------------------------------
%%% @author gjasinski
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 17. mar 2017 13:31
%%%-------------------------------------------------------------------
-module(main).
-author("gjasinski").

%% API
-export([
  power/2,
  contains/2,
  duplicateElements/1,
  divisibleBy/2,
  toBinary/1
]).


power(_,0) -> 1;
power(A,N) -> A * power(A,N-1).

contains([],_) -> false;
contains([Head|Tail], A) ->
  if
    Head == A -> true;
    Head /= A -> contains(Tail, A)
  end.

duplicateElements([]) -> [];
duplicateElements([Head]) -> [Head, Head];
duplicateElements([Head|Tail]) -> [Head, Head] ++ duplicateElements(Tail).

divisibleBy([],_) -> [];
divisibleBy([Head|Tail], Divisor) ->
  if
    Head rem Divisor == 0 -> [Head] ++ divisibleBy(Tail, Divisor);
    Head rem Divisor /= 0 -> divisibleBy(Tail, Divisor)
  end.

toBinary(0) -> [0];
toBinary(N) -> toBinary(N div 2) ++ [N rem 2].

