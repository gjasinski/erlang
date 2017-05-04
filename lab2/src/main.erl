%%%-------------------------------------------------------------------
%%% @author gjasinski
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 31. mar 2017 13:09
%%%-------------------------------------------------------------------
-module(main).
-author("gjasinski").

%% API
-export([ lessThan/2,
          grtEqThan/2,
          qs/1,
          randomElems/3,
          compareSpeeds/3,
          map/2,
          filter/2,
          foldl/3,
          len/1,
          sum/1,
          sumToMil/0
  ]).

lessThan([], _) -> [];
lessThan([Head|Tail], Arg) when Head < Arg -> [Head] ++ lessThan(Tail, Arg);
lessThan([_|Tail], Arg) -> lessThan(Tail, Arg).

grtEqThan([], _) -> [];
grtEqThan([Head|Tail], Arg) when Head >= Arg -> [Head] ++ grtEqThan(Tail, Arg);

grtEqThan([_|Tail], Arg) -> grtEqThan(Tail, Arg).

qs([]) -> [];
qs([Pivot|Tail]) -> qs( lessThan(Tail,Pivot) ) ++ [Pivot] ++ qs( grtEqThan(Tail,Pivot) ).

randomElems(N,Min,Max) -> [random:uniform(Max - Min) + Min || X <- lists:seq(1, N)].

compareSpeeds(List, Fun1, Fun2) -> timer:tc(Fun1, List).

filter(F, List) -> [X || X <- List, F(X)].
map(F, List) -> [F(X) || X <- List].
foldl(F, Acc, [Head|Tail]) -> foldl(F, F(Acc, Head), Tail);
foldl(F, Acc, []) -> Acc.
len(Num) -> lists:foldl(fun (X, Y) -> Y + 1 end, 0, integer_to_list(Num, 10)).
sum(Num) -> Num rem 3 + sum(Num div 3).
sumToMil() -> lists:filter(fun (X) -> X rem 3 == 0 end, randomElems(1000000, 0, 1000)).
