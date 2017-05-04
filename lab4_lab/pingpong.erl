-module(pingpong).
-export([start/0, stop/0, play/1, ping/0, pong/0]).

play(I) ->
	send_ping ! {I}.

ping() ->
	receive
		terminate -> ok;
		{I} -> io:format("Ping ~b~n", [I]),
				timer:sleep(1000),
				if
					I > 0 -> send_pong ! {I - 1},
										ping();
					true -> ping()
				end

	after
		20000 -> ok
	end.

pong() ->
	receive
		terminate -> ok;
		{I} -> io:format("Pong ~b~n", [I]),
			timer:sleep(1000),
			if
				I > 0 -> send_ping !{I - 1},
									ping();
				true -> ping()
			end
	after
		20000 -> ok
	end.

start() ->
	P1 = spawn(fun ping/0),
	P2 = spawn(fun pong/0),
	register(send_ping, P1),
	register(send_pong, P2).



stop() ->
	send_ping ! terminate,
	send_pong ! terminate	.


%rebar rompile
%eunit -> makra erlangowe
%github.com/pkociepka/erlang
%src/pollution.erl
%kazda funckja _test
%_ifdef(test)
%endif
