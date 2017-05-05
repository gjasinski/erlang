-module(pfactorial).
-export([seq/1,conc/1,smart/1]).
-export([conc/2,factorial/1,factorial/2,grab/3,loop/0,smart/2,smart_process/2]).

seq(0) -> 1;
seq(N) -> [factorial(X) || X<-lists:seq(0, N)].

factorial(0) -> 1;
factorial(N) -> N * factorial(N-1).
factorial(Pid, N) -> Pid ! {N, factorial(N)}.

conc(N) ->
	register(grabber, spawn(?MODULE, grab, [0, N, []])),
	pfactorial:conc(0, N).
conc(0, N) ->
	factorial(whereis(grabber), 0),
	pfactorial:conc(1, N);
conc(X, N) when X==N ->
	spawn(?MODULE, factorial, [whereis(grabber), N]),
	ok;
conc(X, N) ->
	spawn(?MODULE, factorial, [whereis(grabber), X]),
	pfactorial:conc(X+1, N).

grab(I, N, List) ->
	if
		I > N -> ok;%io:fwrite("~w~n", [List]);
		I =< N ->
			receive
				{I, Numb} -> pfactorial:grab(I+1, N,List++[Numb])
			end
	end.

smart(N) ->
	register(grabber, spawn(?MODULE, grab, [0, N, []])),
	pfactorial:smart(N, erlang:system_info(logical_processors_available)).
smart(N, C) ->
	lists:foreach(fun(I) -> spawn(?MODULE, smart_process, [N-I, C]) end, lists:seq(0, C - 1)).
	
smart_process(N, _) when N<0 -> ok;
smart_process(N, C) ->
	grabber ! {N, factorial(N)},
	smart_process(N-C, C).
	
loop() ->
	receive
		N when is_integer(N) -> 
			pfactorial:factorial(whereis(grabber), N),
			pfactorial:loop();
		stop -> stopped
	end. 