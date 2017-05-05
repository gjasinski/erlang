-module(rAddressBook).
-export([start/0, addContact/2, addEmail/3, addPhone/3, removeContact/2, removeEmail/1, removePhone/1, getEmails/2, getPhones/2, getByEmail/1, getByPhone/1, friendlyByName/2, crash/0]).
-export([init/0, loop/1]).

start() ->
	register(addressBookServer, spawn_link(?MODULE, init, [])).

init() ->
	loop(addressBook:createAddressBook()).
 
addContact(Fname, Lname) ->
	addressBookServer ! {add_contact, Fname, Lname, self()},
	receiveAnswer().

addEmail(Fname, Lname, Email) ->
	addressBookServer ! {add_email, Lname, Fname, Email, self()},
	receiveAnswer().
 
addPhone(Fname, Lname, Phone) ->
	addressBookServer ! {add_email, Fname, Lname, Phone, self()},
	receiveAnswer().

removeContact(Fname, Lname) ->
	addressBookServer ! {remove_contact, Fname, Lname, self()},
	receiveAnswer().

removeEmail(Email) ->
	addressBookServer ! {remove_email, Email, self()},
	receiveAnswer().

removePhone(Phone) ->
	addressBookServer ! {remove_phone, Phone, self()},
	receiveAnswer().

getEmails(Fname, Lname) ->
	addressBookServer ! {get_emails, Fname, Lname, self()},
	receiveAnswer().
 
getPhones(Fname, Lname) ->
	addressBookServer ! {get_phones, Fname, Lname, self()},
	receiveAnswer().

getByEmail(Email) ->
	addressBookServer ! {get_by_email, Email, self()},
	receiveAnswer().

getByPhone(Phone) ->
	addressBookServer ! {get_by_phone, Phone, self()},
	receiveAnswer().

friendlyByName(Fname, Lname) ->
	addressBookServer ! {friendly_by_name, Fname, Lname, self()},
	receiveAnswer().

crash() ->
	addressBookServer ! crash.

loop(AddressBook) ->
	receive
		{add_contact, Fname, Lname, Pid} ->	loop(check(AddressBook, Pid, addressBook:addContact(Fname, Lname, AddressBook)));
		{add_email, Fname, Lname, Email, Pid} -> loop(check(AddressBook, Pid, addressBook:addEmail(Fname, Lname, Email, AddressBook)));
		{add_phone, Fname, Lname, Phone, Pid} -> loop(check(AddressBook, Pid, addressBook:addPhone(Fname, Lname, Phone, AddressBook)));
		{remove_contact, Fname, Lname, Pid} -> loop(check(AddressBook, Pid, addressBook:removeContact(Fname, Lname, AddressBook)));
		{remove_email, Email, Pid} -> loop(check(AddressBook, Pid, addressBook:removeEmail(Email, AddressBook)));
		{remove_phone, Phone, Pid} -> loop(check(AddressBook, Pid, addressBook:removePhone(Phone, AddressBook)));
		{get_emails, Fname, Lname, Pid} ->
			Pid ! addressBook:getEmails(Fname, Lname, AddressBook),
			loop(AddressBook);
		{get_phones, Fname, Lname, Pid} ->
			Pid ! addressBook:getPhones(Fname, Lname, AddressBook),
			loop(AddressBook);
		{get_by_email, Email, Pid} ->
			Pid ! addressBook:getByEmail(Email, AddressBook),
			loop(AddressBook);
		{get_by_phone, Phone, Pid} ->
			Pid ! addressBook:getByPhone(Phone, AddressBook),
			loop(AddressBook);
		{friendly_by_name, Fname, Lname, Pid} ->
			Pid ! addressBook:friendlyByName(Fname, Lname, AddressBook),
			loop(AddressBook);
		crash ->
			2/0
	end.

check(AddressBook, Pid, {error, Description}) ->
	Pid ! {error, Description},
	AddressBook;
check(_, Pid, NewAddressBook) ->
	Pid ! ok,
	NewAddressBook.

receiveAnswer() ->
	receive
		_ -> ok
	end.