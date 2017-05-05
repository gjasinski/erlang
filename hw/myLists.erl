-module(myLists).
-export([contains/2,duplicateElements/1,sumFloats/1]).

contains([],_) -> false;
contains([H|T],H) -> true;
contains([H|T],V) -> contains(T,V).

duplicateElements([]) -> [];
duplicateElements([H|T]) -> [H,H|duplicateElements(T)].
	
sumFloats(List) -> sumFloats(List,0).
sumFloats([],Sum) -> Sum;
sumFloats([H|T],Sum) when is_float(H) -> sumFloats(T,Sum+H);
sumFloats([H|T],Sum) -> sumFloats(T,Sum).