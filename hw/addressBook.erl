-module(addressBook).
-export([createAddressBook/0,addContact/3,addEmail/4,addPhone/4,removeContact/3,removeEmail/2,removePhone/2,getEmails/3,getPhones/3,findByEmail/2,findByPhone/2,friendlyByName/3]).
-record(contact, {fname, lname, phone=[], mail=[]}). 

createAddressBook() -> [].

addContact(Fname, Lname, AB) ->
	case getContact(Fname, Lname, AB) of
		false -> [#contact{fname=Fname, lname=Lname}|AB];
		_ -> {error, "Taki kontakt ju¿ istnieje."}
	end.
getContact(_, _, []) -> false;
getContact(Fname, Lname, [#contact{fname=Fname, lname=Lname}|_]) -> true;
getContact(Fname, Lname, [_|T]) -> getContact(Fname, Lname, T).

emailUnique(_, []) -> true;
emailUnique(Email, [H|T]) -> 
	case emailUniqueForContact(Email, H#contact.mail) of 
		true -> emailUnique(Email, T);
		false -> false
	end.
emailUniqueForContact(_, []) -> true;
emailUniqueForContact(Email, [Email|_]) -> false;
emailUniqueForContact(Email, [_|T]) -> emailUniqueForContact(Email, T).

addEmail(Fname, Lname, Email, AB) -> 
	case emailUnique(Email, AB) of
		true -> actuallyAddEmail(Fname, Lname, Email, AB);
		false -> {error,"Email ju¿ istnieje"}
	end.
actuallyAddEmail(Fname, Lname, Email, []) ->[#contact{fname=Fname, lname=Lname, mail=[Email]}];
actuallyAddEmail(Fname, Lname, Email, [H=#contact{fname=Fname, lname=Lname}|T]) ->
	[#contact{fname=Fname,lname=Lname,phone=H#contact.phone,mail=H#contact.mail++[Email]}|T];
actuallyAddEmail(Fname, Lname, Email, [H|T]) -> [H|actuallyAddEmail(Fname, Lname, Email, T)].

phoneUnique(_, []) -> true;
phoneUnique(Phone, [H|T]) -> 
	case phoneUniqueForContact(Phone, H#contact.phone) of 
		true -> phoneUnique(Phone, T);
		false -> false
	end.
phoneUniqueForContact(_, []) -> true;
phoneUniqueForContact(Phone, [Phone|_]) -> false;
phoneUniqueForContact(Phone, [_|T]) -> phoneUniqueForContact(Phone, T).

addPhone(Fname, Lname, Phone, AB) -> 
	case phoneUnique(Phone, AB) of
		true -> actuallyAddPhone(Fname, Lname, Phone, AB);
		false -> {error,"Numer ju¿ istnieje"}
	end.
actuallyAddPhone(Fname, Lname, Phone, []) ->[#contact{fname=Fname, lname=Lname, phone=[Phone]}];
actuallyAddPhone(Fname, Lname, Phone, [H=#contact{fname=Fname, lname=Lname}|T]) ->
	[#contact{fname=Fname,lname=Lname,phone=H#contact.phone++[Phone],mail=H#contact.mail}|T];
actuallyAddPhone(Fname, Lname, Phone, [H|T]) -> [H|actuallyAddPhone(Fname, Lname, Phone, T)].

removeContact(Fname, Lname, AB) ->
	case getContact(Fname, Lname, AB) of
		true -> doRemoveContact(Fname, Lname, AB);
		_ -> {error, "Taki kontakt nie istnieje w bazie."}
	end.
doRemoveContact(_, _, []) -> [];
doRemoveContact(Fname, Lname, [#contact{fname=Fname, lname=Lname}|T]) -> T;
doRemoveContact(Fname, Lname, [H|T]) -> [H|removeContact(Fname, Lname, T)].

removeEmail(Email, AB) ->
	case isEmail(Email, AB) of
		true -> doRemoveEmail(Email, AB);
		_ -> {error, "Nie ma takiego maila w bazie."}
	end.
doRemoveEmail(_, []) -> [];
doRemoveEmail(Email, [H|T]) -> [#contact{fname=H#contact.fname, lname=H#contact.lname, phone=H#contact.phone, mail=[X || X<-H#contact.mail, X/=Email]}|doRemoveEmail(Email, T)].
isEmail(_, []) -> false;
isEmail(Email, [H|T]) ->
	case findItForUser(Email, H#contact.mail) of
		true -> true;
		_ -> isEmail(Email, T)
	end.

removePhone(Phone, AB) ->
	case isPhone(Phone, AB) of
		true -> doRemovePhone(Phone, AB);
		_ -> {error, "Nie ma takiego telefonu w bazie."}
	end.
doRemovePhone(_, []) -> [];
doRemovePhone(Phone, [H|T]) -> [#contact{fname=H#contact.fname, lname=H#contact.lname, phone=[X || X<-H#contact.phone, X/=Phone], mail=H#contact.mail}|doRemovePhone(Phone, T)].
isPhone(_, []) -> false;
isPhone(Phone, [H|T]) ->
	case findItForUser(Phone, H#contact.phone) of
		true -> true;
		_ -> isPhone(Phone, T)
	end.

getEmails(_, _, []) -> {error, "Nie znaleziono takiej osoby."};
getEmails(Fname, Lname, [H=#contact{fname=Fname, lname=Lname}|_]) -> H#contact.mail;
getEmails(Fname, Lname, [_|T]) -> getEmails(Fname, Lname, T).

getPhones(_, _, []) -> {error, "Nie znaleziono takiej osoby."};
getPhones(Fname, Lname, [H=#contact{fname=Fname, lname=Lname}|_]) -> H#contact.phone;
getPhones(Fname, Lname, [_|T]) -> getPhones(Fname, Lname, T).

findByEmail(_,[]) -> {error,"Nie znaleziono takiego maila"};
findByEmail(Email,[H|T]) -> 
	case findItForUser(Email, H#contact.mail) of
		true -> {H#contact.fname, H#contact.lname};
		false -> findByEmail(Email, T)
	end.
	
findByPhone(_,[]) -> {error,"Nie znaleziono takiego telefonu"};
findByPhone(Phone,[H|T]) -> 
	case findItForUser(Phone, H#contact.phone) of
		true -> {H#contact.fname, H#contact.lname};
		false -> findByPhone(Phone, T)
	end.
	
findItForUser(_, []) -> false;
findItForUser(It, [It|_]) -> true;
findItForUser(It, [_|T]) -> findItForUser(It, T).

friendlyByName(_, _, []) -> "Wystapil blad: taka osoba nie istnieje.";
friendlyByName(Fname, Lname, [H=#contact{fname=Fname, lname=Lname}|_]) ->
	io:fwrite("Imie: ~s~nNazwisko: ~s~nTelefon: ~ts~nMail: ~ts~n", [H#contact.fname,H#contact.lname,io_lib:write(H#contact.phone),io_lib:print(H#contact.mail)]);
friendlyByName(Fname, Lname, [_|T]) -> friendlyByName(Fname, Lname, T).