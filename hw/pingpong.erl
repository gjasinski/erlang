-module(pingpong).
-export([start/0, stop/0, play/1]).
-export([loop/0]).

start() -> 
	register(ping, spawn(?MODULE, loop, [])),
	register(pong, spawn(?MODULE, loop, [])).

stop() -> 
	ping ! stop,
	pong ! stop.

play(N) ->
	ping ! {pong, N}. 

loop() -> 
	receive
		{_, -1} -> pingpong:loop();
		{Pid, N} -> 
			timer:sleep(1000),
			{_, Name} = process_info(self(), registered_name),
			io:format("In \"~s\". To send: ~p~n", [Name, N]),
			Pid ! {self(), N-1},
			pingpong:loop();
		stop -> stopped
		after 20000 -> ok
	end.