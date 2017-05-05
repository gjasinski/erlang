-module(rAddressBookSup).
-export([start_link/0, stop/0]).
-export([init/0, loop/0]).

start_link() ->
	register(support, spawn(?MODULE, init, [])).
 
init() ->	
	rAddressBookSup:loop().
	
stop() -> support ! stop.

loop() ->
	process_flag(trap_exit, true),
	rAddressBook:start(),
	receive
		{'EXIT', _, _} ->
			rAddressBookSup:loop();
		stop -> ok
	end.