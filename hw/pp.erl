-module(pp).
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
    {Pid, N} -> 
      timer:sleep(1000),
      {registered_name, Name} = process_info(self(), registered_name),
      io:format("In \"~s\". Messages to go: ~p~n", [Name, N]),
      send(self(), Pid, N-1),
      pingpong:loop();
    stop -> 
      stopped
  after
    20000 ->
      killed
  end.

send(_, _, -1) ->
  done;
send(From, To, N) ->
  To ! {From, N}.