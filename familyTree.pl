:- dynamic person/6.
:- dynamic wife/2.
:- dynamic husband/2.
:- discontiguous processMainMenuChoice/1.
:- discontiguous person/6.
:- discontiguous level/2.
:- dynamic level/2.
:- dynamic levelWrite/2.
:- dynamic wife/2.
:- dynamic husband/2.
:- dynamic female/1.
:- dynamic male/1.
:- discontiguous wife/2.
:- discontiguous husband/2.

main :-
    printMainMenu,
    getChoice(Choice),
    (Choice == 6 -> ! ; processMainMenuChoice(Choice)).

printMainMenu() :-
    write('1-Ask Relation'), nl,
    write('2-Add/Update Person'), nl,
    write('3-Get Information of Any Person'), nl,
    write('4-Print The Family Tree'), nl,
    write('5-Add Marriage'), nl,
    write('6-Terminate The Program'), nl,
    write('Please choose an operation!'), nl.

getChoice(Ch) :-
  read(Ch).

processMainMenuChoice(1) :-
    write('Please type first person name and surname: '),nl,
    read(FirstPerson),
    write('Please type second person name and surname: '),nl,
    read(SecondPerson),
    ( baba(FirstPerson, SecondPerson)
    -> write('Baba')
    ; anne(FirstPerson, SecondPerson)
    -> write('Anne')
    ; erkek_kardes(FirstPerson, SecondPerson)
    -> write('Erkek kardes')
    ; abi(FirstPerson, SecondPerson)
    -> write('Abi')
    ; abla(FirstPerson, SecondPerson)
    -> write('Abla')
    ; kiz_kardes(FirstPerson, SecondPerson)
    -> write('Kiz kardes')
    ; baldiz(FirstPerson, SecondPerson)
    -> write('Baldiz')
    ; bacanak(FirstPerson, SecondPerson)
    -> write('Bacanak')
    ; yegen(FirstPerson, SecondPerson)
    -> write('Yegen')
    ; kuzen(FirstPerson, SecondPerson)
    -> write('Kuzen')
    ; eniste(FirstPerson, SecondPerson)
    -> write('Eniste')
    ; yenge(FirstPerson, SecondPerson)
    -> write('Yenge')
    ; elti(FirstPerson, SecondPerson)
    -> write('Elti')
    ; kayinbirader(FirstPerson, SecondPerson)
    -> write('Kayinbirader')
    ; amca(FirstPerson, SecondPerson)
    -> write('Amca')
    ; hala(FirstPerson, SecondPerson)
    -> write('Hala')
    ; dayi(FirstPerson, SecondPerson)
    -> write('Dayi')
    ; teyze(FirstPerson, SecondPerson)
    -> write('Teyze')
    ; kayinvalide(FirstPerson, SecondPerson)
    -> write('Kayinvalide')
    ; kayinpeder(FirstPerson, SecondPerson)
    -> write('Kayinpeder')
    ; gelin(FirstPerson, SecondPerson)
    -> write('Gelin')
    ; damat(FirstPerson, SecondPerson)
    -> write('Damat')
    ; (wife(FirstPerson,SecondPerson); husband(FirstPerson,SecondPerson))
    -> write('Es')
    ; (anne(SecondPerson,FirstPerson);baba(SecondPerson,FirstPerson))
    -> write('Cocuk')
    ; (grandparent(FirstPerson,SecondPerson))
    -> write('Grandparent')
    ), nl,
    main.

processMainMenuChoice(2) :-
    write('1-Add person'),nl,
    write('2-Update person'),nl,
    write('Please choice an operation! '),nl,
    getChoice(Choice2),
    processSecondMenuChoice(Choice2),nl,
    main.

processSecondMenuChoice(1) :-
    write('Please type the father name and surname: '),nl,
    read(FatherName),
    write('Please type the mother name and surname: '),nl,
    read(MotherName),
    write('Please type the child name and surname: '),nl,
    read(ChildName),
     write('Please type the birthdate of the child: '),nl,
    read(BirthDate),
    write('Please type the death date of the child: '),nl,
    read(DeathDate),
    write('Please type the child gender: '),nl,
    read(Gender),
    add_person(FatherName, MotherName, Gender, BirthDate, DeathDate, ChildName).

processSecondMenuChoice(2) :-
    repeat,
    write('1.Update the birth year of someone.'),nl,
    write('2.Update the death year of someone.'),nl,
    write('0.Cancel.'),nl,
    write('Enter your choice: '),nl,
    getChoice(UpdateChoice),
    ((UpdateChoice == 0) -> !
    ; processUpdateChoice(UpdateChoice)
    ).



processUpdateChoice(1) :-
    write('Enter the name of person that you want to update: '),nl,
    read(Person21),
    write('Enter the new birthyear:'),nl,
    read(Birthyear),
    updateBirthYear(Person21, Birthyear),
    processSecondMenuChoice(2).

processUpdateChoice(2) :-
    write('Enter the name of person that you want to update: '),nl,
    read(Person22),
    write('Enter the new death year:'),nl,
    read(Deathyear),
    updateDeathYear(Person22, Deathyear),
    processSecondMenuChoice(2).

processMainMenuChoice(3) :-
   ( \+ find_and_assert_all_levels -> true ; true ),
    write('Please type the person name and surname: '),nl,
    getChoice(Person3),
    %person(_, _, _, _, _, Person3),
    getInfo(Person3).

processMainMenuChoice(4) :-
   ( \+ find_and_assert_all_levels -> true ; true ),
    print_all_levels,nl,
    main.

processMainMenuChoice(5) :-
    write('Please type first person name and surname: '),nl,
    read(Person1),
    write('Please type second person name and surname: '),nl,
    read(Person2),
    isMarriageValid(Person1,Person2),
    main.

%FUNCTIONS

add_person(FatherName, MotherName, Gender, BirthDate, DeathDate, PersonName) :-
    ( (Gender == 'f')
        -> assertz(female(PersonName))
    ;   assertz(male(PersonName))
    ),
    assertz(person(FatherName, MotherName, Gender, BirthDate, DeathDate, PersonName)).

add_person_func :-
    write('Please type the father name and surname: '), read(FatherName),
    write('Please type the mother name and surname: '), read(MotherName),
    write('Please type the child name and surname: '), read(ChildName),
    write('Please type the birth year of the child: '), read(BirthDate),
    write('Please type the death year of the child: '), read(DeathDate),
    write('Please type the child gender: '), read(Gender),
    add_person(FatherName, MotherName, Gender, BirthDate, DeathDate, ChildName).


levelFind(Person, Num) :-
    (level(Person, X) ->
        Num = X
    ;
        (parent(Parent, Person) ->
            (level(Parent, ParentLevel) ->
                Num is ParentLevel + 1
            ;
                (wife(Spouse, Person) ; husband(Spouse, Person)),
                (level(Spouse, SpouseLevel) ->
                    Num is SpouseLevel
                ; 
                    Num is 20
                )

            )
        ),
        assertz(level(Person, Num))
    ).

levelWrite(Person, Num) :-
    levelFind(Person, Num).

find_and_assert_all_levels :-
    setof(Person, Parent^(parent(Parent, Person)), AllPersons),
    forall(member(Person, AllPersons), (levelFind(Person, _))),
    setof(Parent, Child^(parent(Parent, Child)), AllParents),
    forall(member(Parent, AllParents), (levelFind(Parent, _))),
    findall(Spouse1, Spouse2^(wife(Spouse1, Spouse2)), AllSpouses1),
    forall(member(Spouse1, AllSpouses1), (levelFind(Spouse1, _))),
    findall(Spouse2, Spouse1^(husband(Spouse2, Spouse1)), AllSpouses2),
    forall(member(Spouse2, AllSpouses2), (levelFind(Spouse2, _))).

print_level_0 :-
    findall(Person, level(Person, 0), PersonsWithLevel0),
    writeln('Persons with level 0:'),
    forall(member(Person, PersonsWithLevel0), writeln(Person)).

print_all_levels :-
    setof(Level, Person^level(Person, Level), Levels),
    forall(member(Level, Levels),
        (
            findall(Person, level(Person, Level), Persons),
            format('-----Level ~w-----', [Level]), nl,
            print_persons_with_spouses(Persons, [])
        )
    ).

find_spouse(Person, Spouse) :-
    (wife(Person, Spouse) ; husband(Spouse, Person)).

print_persons_with_spouses([], _Printed).
print_persons_with_spouses([Person|Rest], Printed) :-
    (member(Person, Printed) ->
        print_persons_with_spouses(Rest, Printed)
    ;
        (find_spouse(Person, Spouse) ->
            format('~w - ~w', [Person, Spouse]), nl,
            print_persons_with_spouses(Rest, [Person, Spouse | Printed])
        ;
            writeln(Person),
            print_persons_with_spouses(Rest, [Person | Printed])
        )
    ).


getInfo(Person) :-
  calculateAge(Person, Age),
  write('Age:') , write(Age),nl,
  level(Person,X),
  write('Level:'), write(X),nl,
  countChildren(Person, Count),
  write('Total child: '), write(Count),nl,
  printIsAlive(Person),nl,
  main.

countChildren(Person, Count) :-
    findall(Child, person(Person, _, _, _, _, Child), Children),
    length(Children, Count).

calculateAge(Person, Age) :-
    person(_, _, _, BirthYear, DeathYear, Person),
    (DeathYear == 'none' ->
    (get_time(Today),
    stamp_date_time(Today, Date, 'UTC'),
    date_time_value(year, Date, CurrentYear),
    Age is CurrentYear - BirthYear)
    ;
    Age is DeathYear - BirthYear).

isAlive(Person) :-
    person(_, _, _, _, DeathDate, Person),
    DeathDate == 'none'.

printIsAlive(Person) :-
    (   isAlive(Person)
    ->  write('Alive.'), nl
    ;   write('Not alive.'), nl
    ).

updateBirthYear(PersonName,NewBirthYear) :-
    retract(person(Father, Mother, Gender, _, DeathDate, PersonName)),
    assertz(person(Father, Mother, Gender, NewBirthYear, DeathDate, PersonName)),
    write('Birth year updated successfully for '), write(PersonName), write('.'), nl.

updateDeathYear(PersonName,NewDeathYear) :-
    retract(person(Father, Mother, Gender, BirthDate, _, PersonName)),
    assertz(person(Father, Mother, Gender, BirthDate, NewDeathYear, PersonName)),
    write('Death year updated successfully for '), write(PersonName), write('.'), nl.

addMarriage(Person1, Person2) :-
    assertz(wife(Person1, Person2)),
    assertz(husband(Person2, Person1)).

isMarriageValid(Person1,Person2) :-
    calculateAge(Person1, Age1),
    calculateAge(Person2, Age2),
    (wife(Person1,_);husband(Person1,_);wife(Person2,_);husband(Person2,_)
    ->  write('Already married'),nl
    ;
        ((isAlive(Person1), isAlive(Person2))
        ->
        (   (Age1 < 18 ; Age2 < 18)
        ->  write('Marriage is invalid due to age restriction.'), nl
        ;   is_relative(Person1, Person2)
        ->  nl
        ;   write('Marriage is valid.'),nl, addMarriage(Person1,Person2)
        )
        ;
          write('Not alive!!!'))
    ).

is_relative(FirstPerson, SecondPerson):-
    ( baba(FirstPerson, SecondPerson)
    -> write('invalid marriage !'),nl,write('Baba')
    ; anne(FirstPerson, SecondPerson)
    -> write('invalid marriage !'),nl,write('Anne')
    ; erkek_kardes(FirstPerson, SecondPerson)
    -> write('invalid marriage !'),nl,write('Erkek kardes')
    ; abi(FirstPerson, SecondPerson)
    -> write('invalid marriage !'),nl,write('Abi')
    ; abla(FirstPerson, SecondPerson)
    -> write('invalid marriage !'),nl,write('Abla')
    ; kiz_kardes(FirstPerson, SecondPerson)
    -> write('invalid marriage !'),nl,write('Kiz kardes')
    ; amca(FirstPerson, SecondPerson)
    -> write('invalid marriage !'),nl,write('Amca')
    ; hala(FirstPerson, SecondPerson)
    -> write('invalid marriage !'),nl,write('Hala')
    ; dayi(FirstPerson, SecondPerson)
    -> write('invalid marriage !'),nl,write('Dayi')
    ; teyze(FirstPerson, SecondPerson)
    -> write('invalid marriage !'),nl,write('Teyze')
    ; grandparent(FirstPerson, SecondPerson)
    -> write('invalid marriage !'),nl,write('Grandparent')
    ).
%RELATIONS

isOlder(X, Y) :- person(_, _, _, BirthDateX, _, X), person(_, _, _, BirthDateY, _, Y), BirthDateX < BirthDateY.

siblings(Person1, Person2) :-
    person(FatherName, MotherName, _, _, _, Person1),
    person(FatherName, MotherName, _, _, _, Person2),
    Person1 \= Person2.

baba(FatherName, PersonName) :- person(FatherName, _, _, _, _, PersonName).
anne(MotherName, PersonName) :- person(_, MotherName, _, _, _, PersonName).
erkek_kardes(X, Y) :- person(Z, W, 'm', _, _, X), person(Z, W, _, _, _, Y), isOlder(Y, X).
abi(X, Y) :- person(Z, W, 'm', _, _, X), person(Z, W, _, _, _, Y), isOlder(X, Y).
abla(X, Y) :- person(Z, W, 'f', _, _, X), person(Z, W, _, _, _, Y), isOlder(X, Y).
kiz_kardes(X, Y) :- person(Z, W, 'f', _, _, X), person(Z, W, _, _, _, Y), isOlder(Y, X).
baldiz(X, Y) :- person(_, _, 'f', _, _, X), person(_, _, 'm', _, _, Y), husband(Y,Z), siblings(Z,X).
bacanak(X, Y) :- person(_, _, 'm', _, _, X), person(_, _, 'm', _, _, Y), husband(X,Z), husband(Y,W), siblings(Z, W).
yegen(X,Y) :-  siblings(Y,Z),parent(Z,X).
kuzen(X,Y) :-  parent(Z,X),parent(W,Y),siblings(Z,W).
eniste(X,Y) :- wife(Z,X),(yegen(Y,Z);siblings(Y,Z)).
yenge(X,Y) :- husband(Z,X),(yegen(Y,Z);siblings(Y,Z)).
elti(X,Y) :- wife(Z,X),wife(W,Y),siblings(Z,W).
kayinbirader(X,Y) :- (wife(Z,Y), abi(X,Z));(husband(Z,Y), abi(X,Z)).
kayinbirader(X,Y) :- (wife(Z,Y), erkek_kardes(X,Z));(husband(Z,Y), erkek_kardes(X,Z)).

parent(X,Y) :- baba(X,Y); anne(X,Y).
% husband(X,Y) :- male(X) , female(Y) , parent(X,Z) , parent(Y,Z).
% wife(X,Y) :- female(X) , male(Y) , parent(X,Z) , parent(Y,Z).
amca(X,Y):-male(X) , abi(X,Z) , male(Z) , parent(Z,Y).
amca(X,Y):-male(X) , erkek_kardes(X,Z) , male(Z) , parent(Z,Y).
hala(X,Y):-female(X) , kiz_kardes(X,Z) , male(Z) , parent(Z,Y).
hala(X,Y):-female(X) , abla(X,Z) , male(Z) , parent(Z,Y).
dayi(X,Y):-male(X) , abi(X,Z) , female(Z) , parent(Z,Y).
dayi(X,Y):-male(X) , erkek_kardes(X,Z) , female(Z) , parent(Z,Y).
teyze(X,Y):-female(X) , abla(X,Z) , female(Z) , parent(Z,Y).
teyze(X,Y):-female(X) , kiz_kardes(X,Z) , female(Z) , parent(Z,Y).
kayinvalide(X,Y):-female(X) , anne(X,Z) , female(Z) , husband(Y,Z).
kayinvalide(X,Y):-female(X) , anne(X,Z) , male(Z) , wife(Y,Z).
kayinpeder(X,Y):-male(X) , baba(X,Z) , female(Z) , husband(Y,Z).
kayinpeder(X,Y):-male(X) , baba(X,Z) , male(Z) , wife(Y,Z).
gelin(X,Y):-female(X) , parent(Y,Z) , husband(Z,X).
damat(X,Y):-male(X) , parent(Y,Z) , wife(Z,X).
grandparent(X,Y):- parent(X,Z),parent(Z,Y).

person('x', 'y', 'f', 1985, 'none', 'Sedanur Aslan').
level('Sedanur Aslan',0).
