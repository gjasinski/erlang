-module(addressBookSupervisorOtp).
-version('1.0').
-behaviour(supervisor).

-export([start_link/0,init/1]).

start_link() -> 
	supervisor:start_link({local, addressBookSupervisorOtp}, ?MODULE, []).
	
init(_) ->
	{ok, {{one_for_all, 2, 2000},[{rAddressBookSupOtp,{rAddressBookSupOtp, start_link, []},permanent, brutal_kill, worker, [rAddressBookSupOtp]}]}}.