-module(qsort).
-export([qsort/1]).

qsort([]) -> [];
qsort([Pivot|Tail]) -> qsort([X || X<-Tail, X<Pivot]) ++ [Pivot] ++ qsort([X || X<-Tail, X>=Pivot]).