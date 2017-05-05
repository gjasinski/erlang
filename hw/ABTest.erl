AB=addressBook:createAddressBook().
AB1=addressBook:addContact("Bartosz","Rakoczy",AB).
AB2=addressBook:addContact("Michal","Rakoczy",AB1).
AB3=addressBook:addContact("A","B",AB2).
ERT=addressBook:addContact("A","B",AB3).
AB4=addressBook:addEmail("Czeslaw","Nie-man","abcd@abd.pl",AB3).
AB5=addressBook:addEmail("A","B","xvc.pl",AB4).
AB6=addressBook:addEmail("A","C","xxx.xx",AB5).
AB7=addressBook:addEmail("Andrzej","C","asd.pl",AB6).
AB8=addressBook:addEmail("A","B","123@pl.pl",AB7).
AB9=addressBook:addEmail("A","B","1234@pl.pl",AB8).
AB10=addressBook:addEmail("Bartosz","Rakoczy","xxx.xx",AB9).
AB11=addressBook:addPhone("Adrian","Japko",1234,AB9).
AB12=addressBook:addPhone("Bartosz","Rakoczy",1234,AB11).
AB13=addressBook:addPhone("Bartosz","Rakoczy",21324,AB11).
AB14=addressBook:addPhone("A","B",9876,AB13).
AB15=addressBook:addPhone("A","B",999,AB14).
AB16=addressBook:addPhone("A","B",997,AB15).
AB17=addressBook:addPhone("A","C",998,AB16).
AB18=addressBook:removeContact("Michal","Rakoczy",AB17).
AB19=addressBook:removeContact("Bartosz","Rakoczy",AB18).
ERT2=addressBook:removeContact("qwerty", "uiop", AB19).
AB20=addressBook:removeEmail("asd.pl",AB19).
AB21=addressBook:removeEmail("xvc.pl",AB20).
ERT3=addressBook:removeEmail("xvc.pl",AB21).
AB22=addressBook:removePhone(998,AB21).
AB23=addressBook:removePhone(999,AB22).
ERT4=addressBook:removePhone(999,AB23).
addressBook:getPhones("A","B",AB23).
addressBook:getEmails("A","B",AB23).
addressBook:findByEmail("123@pl.pl",AB23).
addressBook:findByPhone(997,AB23).
addressBook:friendlyByName("A","B",AB23).
