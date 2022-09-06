%
% Room scheduling

%

:- use_module(library(clpfd)).

time_slot(0,[[m,w,f],[7,10,8,00]]).
time_slot(0,[[m,w,f],[8,10,9,00]]).
time_slot(0,[[m,w,f],[9,10,10,00]]).
time_slot(0,[[m,w,f],[10,10,11,00]]).
time_slot(0,[[m,w,f],[11,10,12,00]]).
time_slot(0,[[m,w,f],[12,10,13,00]]).
time_slot(0,[[m,w,f],[13,10,14,00]]).
time_slot(0,[[m,w,f],[14,10,15,00]]).
time_slot(0,[[m,w,f],[15,10,16,00]]).
time_slot(0,[[m,w,f],[16,10,17,00]]).
time_slot(0,[[m,w,f],[17,10,18,00]]).
time_slot(0,[[m,w,f],[18,10,19,00]]).
time_slot(0,[[m,w,f],[19,10,20,00]]).
time_slot(0,[[m,w,f],[20,10,21,00]]).

time_slot(1,[[t,r],[7,40,9,00]]).
time_slot(1,[[t,r],[8,10,9,30]]).
time_slot(1,[[t,r],[9,40,11,00]]).
time_slot(1,[[t,r],[12,10,13,10]]).
time_slot(1,[[t,r],[13,40,15,00]]).
time_slot(1,[[t,r],[15,10,16,30]]).
time_slot(1,[[t,r],[16,10,17,30]]).
time_slot(1,[[t,r],[16,40,18,00]]).
time_slot(1,[[t,r],[17,10,18,30]]).
time_slot(1,[[t,r],[17,40,19,00]]).
time_slot(1,[[t,r],[18,10,19,30]]).
time_slot(1,[[t,r],[18,40,20,00]]).
time_slot(1,[[t,r],[19,10,20,30]]).
time_slot(1,[[t,r],[19,40,21,00]]).
time_slot(1,[[t,r],[20,10,21,30]]).
time_slot(1,[[t,r],[20,40,22,00]]).

time_slot(2,[[m,t,w,r],[7,10,8,00]]).
time_slot(2,[[m,t,w,r],[8,10,9,00]]).
time_slot(2,[[m,t,w,r],[9,10,10,00]]).
time_slot(2,[[m,t,w,r],[10,10,11,00]]).
time_slot(2,[[m,t,w,r],[12,10,13,00]]).
time_slot(2,[[m,t,w,r],[13,10,14,00]]).
time_slot(2,[[m,t,w,r],[14,10,15,00]]).
time_slot(2,[[m,t,w,r],[15,10,16,00]]).
time_slot(2,[[m,t,w,r],[16,10,17,00]]).
time_slot(2,[[m,t,w,r],[17,10,18,00]]).
time_slot(2,[[m,t,w,r],[18,10,19,00]]).
time_slot(2,[[m,t,w,r],[19,10,20,00]]).
time_slot(2,[[m,t,w,r],[20,10,21,00]]).

% code_insert_here

class(1,[astr-1101-01],2).
class(2,[astr-1101-02],2).
class(3,[astr-1101-03],2).
class(4,[astr-1101-04],2).
class(5,[astr-1102-01],2).
class(6,[astr-1102-02],2).
class(7,[astr-1102-03],2).
class(8,[astr-1200-01],1).
class(9,[astr-1270-01],1).
class(10,[astr-1270-02],1).
class(11,[astr-1270-03],1).
class(12,[astr-1301-01],0).
class(13,[astr-1302-01],1).
class(14,[astr-1302-02],1).
class(15,[astr-1324-01],0).
class(16,[astr-1324-02],0).
class(17,[astr-1324-03],0).
class(18,[astr-1326-01],0).
class(19,[astr-1326-02],0).
class(20,[astr-1326-03],0).
class(21,[astr-1400-01],0).
class(22,[astr-1400-02],0).
class(23,[astr-1400-03],0).
class(24,[astr-1404-01],0).
class(25,[astr-1404-02],0).
class(26,[astr-1404-03],0).
class(27,[astr-1444-01],0).
class(28,[astr-1470-01],1).
class(29,[astr-1470-02],1).
class(30,[astr-1470-03],1).
class(31,[astr-1471-01],1).
class(32,[astr-1471-02],1).
class(33,[astr-1471-03],1).
class(34,[astr-1471-04],1).
class(35,[geol-1200-01],1).
class(36,[geol-1203-01],1).
class(37,[geol-1203-02],1).
class(38,[geol-1203-03],1).
class(39,[geol-1203-04],1).
class(40,[geol-1206-01],0).
class(41,[geol-1241-01],1).
class(42,[geol-1241-02],1).
class(43,[geol-1270-01],1).
class(44,[geol-1270-02],1).
class(45,[geol-1270-03],1).
class(46,[geol-1301-01],0).
class(47,[geol-1301-02],0).
class(48,[geol-1301-03],0).
class(49,[geol-1301-04],0).
class(50,[geol-1303-01],0).
class(51,[geol-1303-02],0).
class(52,[geol-1305-01],0).
class(53,[geol-1305-02],0).
class(54,[geol-1309-01],1).
class(55,[geol-1309-02],1).
class(56,[geol-1330-01],1).
class(57,[geol-1400-01],1).
class(58,[geol-1400-02],1).
class(59,[geol-1404-01],1).
class(60,[geol-1404-02],1).
class(61,[geol-1404-03],1).
class(62,[geol-1415-01],1).
class(63,[geol-1415-02],1).
class(64,[geol-1417-01],0).
class(65,[geol-1420-01],0).
class(66,[geol-1420-02],0).
class(67,[geol-1471-01],0).
class(68,[phys-1100-01],0).
class(69,[phys-1100-02],0).
class(70,[phys-1104-01],0).
class(71,[phys-1104-02],0).
class(72,[phys-1121-01],2).
class(73,[phys-1121-02],2).
class(74,[phys-1121-03],2).
class(75,[phys-1121-04],2).
class(76,[phys-1121-05],2).
class(77,[phys-1121-06],2).
class(78,[phys-1121-07],2).
class(79,[phys-1121-08],2).
class(80,[phys-1121-09],2).
class(81,[phys-1121-10],2).
class(82,[phys-1123-01],1).
class(83,[phys-1125-01],1).
class(84,[phys-1125-02],1).
class(85,[phys-1143-01],1).
class(86,[phys-1200-01],0).
class(87,[phys-1200-02],0).
class(88,[phys-1202-01],1).
class(89,[phys-1211-01],2).
class(90,[phys-1211-02],2).
class(91,[phys-1211-03],2).
class(92,[phys-1220-01],0).
class(93,[phys-1270-01],1).
class(94,[phys-1270-02],1).
class(95,[phys-1305-01],1).
class(96,[phys-1306-01],1).
class(97,[phys-1306-02],1).
class(98,[phys-1306-03],1).
class(99,[phys-1306-04],1).
class(100,[phys-1310-01],0).
class(101,[phys-1310-02],0).
class(102,[phys-1310-03],0).
class(103,[phys-1310-04],0).
class(104,[phys-1313-01],0).
class(105,[phys-1313-02],0).
class(106,[phys-1313-03],0).
class(107,[phys-1313-04],0).
class(108,[phys-1314-01],0).
class(109,[phys-1314-02],0).
class(110,[phys-1315-01],1).
class(111,[phys-1315-02],1).
class(112,[phys-1315-03],1).
class(113,[phys-1315-04],1).
class(114,[phys-1318-01],0).
class(115,[phys-1320-01],0).
class(116,[phys-1320-02],0).
class(117,[phys-1320-03],0).
class(118,[phys-1320-04],0).
class(119,[phys-1321-01],0).
class(120,[phys-1321-02],0).
class(121,[phys-1321-03],0).
class(122,[phys-1323-01],1).
class(123,[phys-1330-01],0).
class(124,[phys-1330-02],0).
class(125,[phys-1342-01],0).
class(126,[phys-1342-02],0).
class(127,[phys-357-01],0).
class(128,[phys-1400-01],1).
class(129,[phys-1400-02],1).
class(130,[phys-1400-03],1).
class(131,[phys-1400-04],1).
class(132,[phys-1401-01],0).
class(133,[phys-1401-02],0).
class(134,[phys-1403-01],0).
class(135,[phys-1404-01],1).
class(136,[phys-1404-02],1).
class(137,[phys-406-01],1).
class(138,[phys-406-02],1).
class(139,[phys-406-03],1).
class(140,[phys-1408-01],1).
class(141,[phys-1408-02],1).
class(142,[phys-409-01],1).
class(143,[phys-409-02],1).
class(144,[phys-1410-01],1).
class(145,[phys-422-01],1).
class(146,[phys-422-02],1).
class(147,[phys-422-03],1).
class(148,[phys-423-01],0).
class(149,[phys-1425-01],0).
class(150,[phys-1425-02],0).
class(151,[phys-1425-03],0).
class(152,[phys-1426-01],1).
class(153,[phys-1426-02],1).
class(154,[phys-1426-03],1).
class(155,[phys-427-01],1).
class(156,[phys-1428-01],0).
class(157,[phys-1461-01],1).
class(158,[phys-1461-02],1).
class(159,[phys-1461-03],1).
class(160,[phys-462-01],1).
class(161,[phys-462-02],1).
class(162,[phys-462-03],1).
class(163,[phys-462-04],1).
class(164,[phys-470-01],1).
class(165,[phys-470-02],1).
class(166,[phys-470-03],1).
class(167,[phys-470-04],1).
class(168,[phys-485-01],0).
class(169,[phys-485-02],0).
class(170,[phys-485-03],0).
class(171,[phys-485-04],0).
class(172,[phys-495-01],1).
class(173,[phys-495-02],1).
class(174,[psc-1101-01],1).
class(175,[psc-1101-02],1).
class(176,[psc-1101-03],1).
class(177,[psc-1101-04],1).
class(178,[psc-102-01],0).
class(179,[psc-102-02],0).
class(180,[psc-102-03],0).
class(181,[psc-102-04],0).
class(182,[psc-1103-01],0).
class(183,[psc-1103-02],0).
class(184,[psc-1103-03],0).
class(185,[psc-1103-04],0).
class(186,[psc-1201-01],0).
class(187,[psc-1320-01],0).
class(188,[psc-1320-02],0).
class(189,[psc-1391-01],1).
class(190,[psc-1391-02],1).
class(191,[psc-1391-03],1).
class(192,[psc-1391-04],1).
class(193,[psc-1424-01],0).
class(194,[psc-1424-02],0).
class(195,[psc-1424-03],0).
class(196,[psc-1491-01],1).
class(197,[psc-1491-02],1).
class(198,[psc-492-01],1).
class(199,[psc-492-02],1).
class(200,[psc-492-03],1).
class(201,[psc-492-04],1).


%must_place_in(1,3).
%must_place_in(201,7).
must_place_in(_,_) :- fail.


:- dynamic room/3.

all_classes_placed :- findall(X,class(X,_,_),AllClasses), findall(Y,room(_,Y,_),PlacedClasses), msort(AllClasses,S), msort(PlacedClasses,S).

times_overlap(T1,T2) :- 
                        nth1(1,T1,StartH1), nth1(2,T1,StartM1), 
                        nth1(3,T1,EndH1), nth1(4,T1,EndM1), 
                        nth1(1,T2,StartH2), nth1(2,T2,StartM2), 
                        nth1(3,T2,EndH2), nth1(4,T2,EndM2),
                        Start1 is StartH1*60+StartM1, End1 is EndH1*60+EndM1, Start2 is StartH2*60+StartM2, End2 is EndH2*60+EndM2, 
                        Start1 =< End2, End1 >= Start2.

pair_overlap([DaysA,TimesA],[DaysB,TimesB]) :- intersection(DaysA,DaysB,S), \+ length(S,0), times_overlap(TimesA,TimesB). 

no_overlap(_,[]).
no_overlap(Proposed,[PlacedHead|PlacedTail]) :- \+ pair_overlap(Proposed,PlacedHead), no_overlap(Proposed,PlacedTail). 


fits_in_room(RoomNum,ProposedDaysTimes) :- findall(X,room(RoomNum,_,X),PlacedSoFar), no_overlap(ProposedDaysTimes,PlacedSoFar). 

json :- write('['), room(A,B,[C|D]), class(B,Class,_), 
        flatten(D,DStr), format('{"room_num":~w,"class_name":"~w","days":"~w","times":"~w"},~n',[A,Class,C,DStr]), fail.
json :- writeln(']').


plan :-
        RoomNum #>= 1, RoomNum #=< 50,
        indomain(RoomNum),
        class(ClassNum,_,TimeSlotGroup),
        time_slot(TimeSlotGroup,DaysTimes),
        fits_in_room(RoomNum,DaysTimes),
        \+ room(_,ClassNum,_),
        assert(room(RoomNum,ClassNum,DaysTimes)),
        all_classes_placed,
        listing(room),
        json.  

plan :- listing(room).

