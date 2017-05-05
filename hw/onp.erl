-module(onp).
-export([onp/1]).

onp(Wyr) ->
	Lista = string:tokens(Wyr," "),
	licz(Lista,[]).

licz([],[H|T]) -> H;
licz(["+"|T],[A,B|T1]) -> licz(T,[A+B|T1]);
licz(["-"|T],[A,B|T1]) -> licz(T,[B-A|T1]);
licz(["*"|T],[A,B|T1]) -> licz(T,[A*B|T1]);
licz(["/"|T],[A,B|T1]) -> licz(T,[B/A|T1]);
licz(["sqrt"|T],[A|T1]) -> licz(T, [math:sqrt(A)|T1]);
licz(["pow"|T],[B,A|T1]) -> licz(T, [math:pow(A,B)|T1]);
licz(["sin"|T],[A|T1]) -> licz(T, [math:sin(A)|T1]);
licz(["cos"|T],[A|T1]) -> licz(T, [math:cos(A)|T1]);
licz(["tan"|T],[A|T1]) -> licz(T, [math:tan(A)|T1]);
licz([H|T],Stos) -> licz(T,[list_to_float(H)|Stos]).