-module(rAddressBookSupOtp).
-version('1.0').
-behaviour(gen_server).
 
-export([start_link/0,init/1,handle_call/3,handle_cast/2,code_change/3,handle_info/2,terminate/2]).
-export([addContact/2,addEmail/3,addPhone/3,removeContact/2,removeEmail/1,removePhone/1,getEmails/2,getPhones/2,getByEmail/1,getByPhone/1,friendlyByName/2,stop/0,crash/0,check/2]).
 
start_link() ->
	gen_server:start_link({local, rAddressBookSupOtp}, ?MODULE, [], []).

init([]) ->
	{ok, []}.
  
addContact(Fname, Lname) ->
	gen_server:call(rAddressBookSupOtp, {add_contact, Fname, Lname}).
 
addEmail(Fname, Lname, Email) ->
	gen_server:call(rAddressBookSupOtp, {add_email, Fname, Lname, Email}).
 
addPhone(Fname, Lname, Phone) ->
	gen_server:call(rAddressBookSupOtp, {add_phone, Fname, Lname, Phone}).
 
removeContact(Fname, Lname) ->
	gen_server:call(rAddressBookSupOtp, {remove_contact, Fname, Lname}).
 
removeEmail(Email) ->
	gen_server:call(rAddressBookSupOtp, {remove_email, Email}).
 
removePhone(Phone) ->
	gen_server:call(rAddressBookSupOtp, {remove_phone, Phone}).
	
getEmails(Fname, Lname) ->
	gen_server:call(rAddressBookSupOtp, {get_emails, Fname, Lname}).
 
getPhones(Fname, Lname) ->
	gen_server:call(rAddressBookSupOtp, {get_phones, Fname, Lname}).
 
getByEmail(Email) ->
	gen_server:call(rAddressBookSupOtp, {get_by_email, Email}).
 
getByPhone(Phone) ->
	gen_server:call(rAddressBookSupOtp, {get_by_phone, Phone}).
 
friendlyByName(Fname, Lname) ->
	gen_server:call(rAddressBookSupOtp, {friendly_by_name, Fname, Lname}).

crash() ->
	gen_server:cast(rAddressBookSupOtp, crash).

stop() ->
	gen_server: cast(rAddressBookSupOtp, stop).

% callbacks:
handle_call({add_contact, Fname, Lname}, _, AddressBook) ->
	check(AddressBook, addressBook:addContact(Fname, Lname, AddressBook));
handle_call({add_email, Fname, Lname, Email}, _, AddressBook) ->
	check(AddressBook, addressBook:addEmail(Fname, Lname, Email, AddressBook));
handle_call({add_phone, Fname, Lname, Phone}, _, AddressBook) ->
	check(AddressBook, addressBook:addPhone(Fname, Lname, Phone, AddressBook));
handle_call({remove_contact, Fname, Lname}, _, AddressBook) ->
	check(AddressBook, addressBook:removeContact(Fname, Lname, AddressBook));
handle_call({remove_email, Email}, _, AddressBook) ->
	check(AddressBook, addressBook:removeEmail(Email, AddressBook));
handle_call({remove_phone, Phone}, _, AddressBook) ->
	check(AddressBook, addressBook:removePhone(Phone, AddressBook));
handle_call({get_emails, Fname, Lname}, _, AddressBook) ->
	{reply, addressBook:getEmails(Fname, Lname, AddressBook), AddressBook};
handle_call({get_phones, Fname, Lname}, _, AddressBook) ->
	{reply, addressBook:getPhones(Fname, Lname, AddressBook), AddressBook};
handle_call({get_by_email, Email}, _, AddressBook) ->
	{reply, addressBook:getByEmail(Email, AddressBook), AddressBook};
handle_call({get_by_phone, Phone}, _, AddressBook) ->
	{reply, addressBook:getByPhone(Phone, AddressBook), AddressBook};
handle_call({friendly_by_name, Fname, Lname}, _, AddressBook) ->
	{reply, addressBook:friendlyByName(Fname, Lname, AddressBook), AddressBook}.
 
handle_cast(stop, AddressBook) ->
	{stop, normal, AddressBook};
handle_cast(crash, AddressBook) ->
	1/0,
	{noreply, AddressBook}.

handle_info(_Message, AddressBook) ->
  {noreply, AddressBook}.
 
code_change(_OldVsn, AddressBook, _Extra) ->
  {ok, AddressBook}.
 
terminate(Reason, _Value) ->
  io:format("Server stopped.~n"),
  Reason.
	
check(AddressBook, {error, Description}) ->
	{reply, {error, Description}, AddressBook};
check(_, NewAddressBook) ->
	{reply, ok, NewAddressBook}.