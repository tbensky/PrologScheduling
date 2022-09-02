# Solving a scheduling problem using Prolog using Constraint Logic Programming (CLP)

This is a simple and step-by-step example of how to use
Prolog and constraint logic programmig (CLP) in a scheduling application.

In this case, we want to schedule many differnet college classes into a minimum number of rooms.

## Funny realization

I've been dabbling with Prolog ever since high school, which is a time span of 30+ years!  After all of this time, I've finally written my first real Prolog program!

The program actually does something useful for me: it helps me with ongoing scheduling work I do as part of my job. 

Pretty long time in coming, no? (You can see some results of my dabbling [here](https://www.codebymath.com/index.php/welcome/lesson_menu#prolog) and [here](https://arxiv.org/abs/2108.09893).)

## First a few thoughts

I find Prolog to be an extremely fun language to use and hack around with, first
becoming aware of it back in '85 when Borland came out with Turbo Prolog 1
and then 2. I owned them both. The Borland manuals were great tutorials on
Prolog (recently bought them all again on eBay). Kudos to the
knowledgable people at Borland who wrote them. 

I look at Prolog on
and off, sometimes with a fair amount of intensity for a few weeks,
then I set it down again for a long time. I've been doing this for the
better part of 30 years! Ouch! I guess hacking with Prolog is kind of a hobby for me.

Whenever I'm able to get Prolog to do something that I need, or have
some "ah ha that's how that works" moment with it, I find it very
rewarding. It always feels like I just learned something.  Prolog
however, has an odd and erroneous reputation though (which is also how I used
to think about it).

### Perception of Prolog

I've read many Stackoverflow questions where someone will say
something like "I need to automate some scheduling" or "I want to add
NLP to my application," or "I need to do X," (where X has some AI-ish
or 'hard' aspects to it, without an obvious procedural language solution).

They'll go on to say "I heard Prolog is really good at this,"
and will post a few lines of `class(intro_to_coding,T,1:30-3:30)` for a scheduling problem or
`is_verb(walk)` for an NLP problem. They'll then throw their hands in the air with a comment
thought like "What do I do now? I thought Prolog is supposed to know
how to handle this."

This is not the case. Prolog (just like your favorite procedural language) may
be able to handle such things, but you still have to tell it how.  All told,
Prolog is a just search language. Its core contains all of the best practices
for matching a pattern (you give it), to some data (that you
also give it). It returns
"true" if it finds a match between the two.  It is generalized search language,
 that uses backtracking for search efficiency.  

The power of Prolog as we see it is twofold:

1. You don't have to go writing the search algorithms yourself. Prolog's are all professional-grade, so you can consider tackling "large" search spaces with Prolog.

2. Prolog makes specifying your data and pattern(s) very natural, maybe even
easier than with a procedural language. Both are just parts of
the actual Prolog code. I usually find specifying the data for a problem very enjoyable, and it feels like I'm making fast progress in solving my problem.  "Prolog is so fun and flexible" I often think. The pattern however, which will drive the search, feels much more difficult, and progress is usually very slow.

I think with Prolog, people get hung up on stating the pattern they are looking for in their data. They 
kind of expect Prolog to just "know" such things. 

In scheduling for
example, there is no built in magic of Prolog like `make_a_schedule(X),` 
where you hit return and X will be a list of rooms and what
classes can be placed into it.  You still have to code up the Prolog
that ensures classes aren't placed on top of each other, some kind of
termination condition, some data structure that models
"rooms of classes," etc. If you want something that may understand scheduing already, look at something like [MiniZinc](https://www.minizinc.org).

As [Triska](https://www.metalevel.at/prolog) pointed out, if you've wandered over to Prolog, it's probably because you know you
have a difficult problem to solve, likely with a large phase-space to explore.
 You're looking at Prolog because even at the start, you are already stuck on what to do. Perhaps you don't
 see an easy route in a procedural language.  Your feeling for problems like these is that if a computer does find a solution for you, it'll
 kind of feel like "magic."

Prolog, at minimum, makes searching large
phase-spaces at least plausible, and the implementation may even be easier on you. 

Likely in your procedural
language, all you can think of is `for(i=0;i<1000000000;i++)`, which you already know won't really work. Prolog
will also be confronted with the "1000000000," but knows how to prune it down into something more tractable, if you 
present your problem to it properly.


# Prolog and Scheduling: Getting Started

## What scheduling need we have

In the scheduling application here, we need to place college classes into available rooms.  We have `N` classrooms and `M` classes to be placed within the rooms.  The number of classes, `M`, can vary, and we'd like to
pack the classes into a minimal number of rooms. In other words, pack in the `M` classes, minimizing `N`.  

The classes are specified (by us), as having a name, number, and
required time slot. 

### Time slots
The time slot comes from a list of times that a class is allowed to be scheduled into. For example, one group of
time slots (group 0, just for labeling sense) might be:

```prolog
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
```

These are the classes that meet 3x/week on Mon, Wed, and Fri.  So any class that is needed to be schedule as a `group 0` class will need to be placed to occupy a Mon, Wed, and Fri, with any
of the specified times, 7:10-8am or 8:10-9am or 9:10am-10am, etc. In other
words, although a `group 0` class  is always 50 min long, meets MWF, and must have a time pattern given; it cannot just be placed at just any time, like MWF, 10:15am-11:05am. 

Here are some more time slots for `group 1` Tues/Thurs (t=tues, r=thurs) classes:


```prolog
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
```

As mentioned above, see how easy it is to put this data into Prolog?

### The classes
Here is a list of how classes, showing how they are specified. These are the classes we need to place in rooms.

```prolog
class(1,[geol-1200-01],1).
class(2,[geol-1203-01],1).
class(3,[geol-1203-02],1).
class(4,[geol-1203-03],1).
class(5,[geol-1203-04],1).
class(6,[geol-1206-01],0).
class(7,[geol-1241-01],1).
class(8,[geol-1241-02],1).
class(9,[geol-1270-01],1).
class(10,[geol-1270-02],1).
class(11,[geol-1270-03],1).
class(12,[geol-1301-01],0).
class(13,[geol-1301-02],0).
```

So we have a few classes (numbered 1-13), with some name, needing to be placed at some time that is consistent with a required time slot group. Here, `geol-1200-01` needs to be placed on Tues/Thurs according to a time in the group 1 time slots.  Class #6, or `geol-1206-1` shall be placed as a group 0 class.

Again, this data went in easily too.

### We're ready

Believe it or not, as complicated as "computer automated scheduling" may sound, that's it for our application.  We tell Prolog about valid time slots, and classes needing to be placed. From this alone, we want Prolog to place the `M` classes (13 here), into `N` rooms, where `N` is a minimum. For us, this is run for `M=200+` classes, simply by extending the `class()` data set.  We also have about 6 or 7 actual time slot groups. 


### Hmmm....scheduling these classes

As a scheduling plan goes, we feel like just which time to choose for a class, within a timeslot group, should be left up to Prolog. Thus, no more specifics on when to place a class will be given.  Many might think trying a random time within a group of time slots is the way to go.  Maybe it is, maybe it isn't. It's not here. (But a random number-based algorithm will place classes in rooms for you, but it has a funny convergence and completion problem.)  Let's just let Prolog grapple with this parameter.

(Note: Genetic algorithms, heavily based on random numbers, can also be used for this. But we had trouble coming up with a way that would allow the algorithm full freedom to cross over and mutate, while also avoiding placing classes on top of one another. Does anyone have any hints in this regard?)



 # Thinking it through

 Here's where the Stackoverflow questions on Prolog alluded to above tend to break down: Given the data above, doesn't Prolog somehow just know what to do? No it does not.  At minimum some "cost" function needs to be specified so any algorithm knows how it's doing (see [Lex/Norvig interview](https://www.youtube.com/watch?v=_VPxEcT_Adc)). In fact cost functions may even become more of the thrust of AI research.  Think: How would Prolog know a cost function for your hard problem? For scheduling, let's think it through. 

Given the data, what would we envision of a successful scheduling algorithm? Here are some key goals:

For each class to be placed:

 * Get its required time slot group.

 * Place it in some room, and when placing doing so, be sure it doesn't conflict (or overlap) with another class already in the room.

 * Only place it once.

 Keep doing this until all classes placed into a room. Expand needed rooms, but being careful not to be wasteful with them (i.e. one class per room would work, but....).


 This is all we can think of for a scheduling algorithm. This is our "cost" function.  Seems like if all of the above is done, we just might have our rooms scheduled! A beauty of
 Prolog now is that these steps more-or-less are the Prolog code. Let's take a look.

## Prolog code: Main goal

As you know, Prolog has goals you tell it to try and resolve with against your data.  Let's translate the above steps into Prolog:

```prolog
:- dynamic room/3.

plan :-
        class(ClassNum,_,TimeSlotGroup),
        time_slot(TimeSlotGroup,DaysTimes),
        fits_in_room(RoomNum,DaysTimes),
        \+ room(_,ClassNum,_),
        assert(room(RoomNum,ClassNum,DaysTimes)),
        all_classes_placed,
        listing(room).
``` 

We'll call our top-level goal `plan` and its logic is as follows:

1. Get a class from the data using `class(ClassNum,_,TimeSlotGroup)`. `ClassNum` will be the class's number (for easy reference), and `TimeSlotGroup` will its required time slot group. This is all just rote data pulling at this point. All we care about at the moment is the class number and what time slot group it requires. So we ignore the class name right now with the `_`. (We could do all of this using a class's name, but for now we'll just stick with its number.)

1. Ok, so given the class requires `TimeSlotGroup`, lets use ` time_slot(TimeSlotGroup,DaysTimes),` to grab some proposed (and specific) days and times (`DaysTimes`) from within that time slot group. We don't know which `DaysTimes` to choose, but feel like this should be part of Prolog's search, so we'll leave that up to Prolog.

1. So we have a class, a proposed day/time pattern and are prepared to place it into a room, whose number is `RoomNum.` Before placing it, let's be sure it doesn't conflict with a class that may already be in the room using `fits_in_room(RoomNum,DaysTimes)`.

1. We use Prolog's built in database `assert()` to keep track of placed classes. But before we assert a class into the database as being "placed," let's be sure we don't do so if it has already been placed using `\+ room(_,ClassNum,_)`. 

1. If a class hasn't been placed, we'll go ahead and place it using `assert(room(RoomNum,ClassNum,DaysTimes))`.

1. Next we check if all classes have been placed using `all_classes_placed`.

1. If so, we list out the room assertions using `listing(room)`, so the user can see the result.

The `:- dynamic room/3.` is a Prolog directive, telling it the assertions of `room` (with 3 parameters) into the database is done at runtime (dynamically); `room` does not have any static representations (like `class` and `timeslot` do).

## The utility routines

We can't run our code just yet, as some calls are not defined, namely `fits_in_room`, and `all_classes_placed`. So we need to develop these functions before the code will run. A few thoughts on this.

### Pure Prolog can be irritating

Many times in our study of Prolog, utility routines are needed. These are short boring-ish routines that help the main search algorithm along, by checking that various conditions are being met. They help to guide the search and ultimately allow it to prune large search spaces.  

For us, this is precisely where we'd usually bail out and abandon our entire Prolog project. Why? Because usually the utililty routines are 

1. Boring.

1. Easy solved by a procedural language.

1. A project in itself, to solve in Prolog.

1. Not impressive or sexy, when we finally figure out how to grind them out in Prolog (they are not AI-ish, not Prology, nothing special). As a case in point, look the Quicksort implemented in Prolog. All we can say is "why," and when done, just post it here (https://www.reddit.com/r/DiWHY/) and find something else to do. Yuck.  Funny though, this is how all books on Prolog in the 80s and 90s were written.  "Let's see if we can implement QuickSort in Prolog." We read all of those books and they were a real turn-off.  This was also the time period that preceeded to the AI-winter. If we see another exercise on seeing if element `X` is in list `Y`.....

More than once, we've wished Prolog would have some kind of embedded Python within it, that could easily take and return Prolog's native data types for some quick processing. You know, allow us to implement some small task, quickly and easily.

We, for example, don't really like recursion. We'd much rather iterate through a Prolog list (for example) with a for-loop, and have tons of the usual C/PHP/Python-type string functions available (trim, strupper, etc.) to us when doing so. All to be passed  neatly back into a Prolog datatype so it may continue its search, which Prolog is what really good at.

### Back at it

With that rant behind us, here's our first needed utility routine, that we would have rather handled procedurally, but by some miracle, we were able to grid out in Prolog: `fits_in_room`, which sees if a proposed day/time pattern called `DaysTimes` fits in room `RoomNum`.  Here's our definition for it:

```prolog
fits_in_room(RoomNum,ProposedDaysTimes) :- 
	findall(X,room(RoomNum,_,X),PlacedSoFar), 
	no_overlap(ProposedDaysTimes,PlacedSoFar).
```

It takes in some `RoomNum` and propopsed time pattern `ProposedDaysTimes`.  It generates a list of all classes placed thus far in room `RoomNum` using `findall(X,room(RoomNum,_,X),PlacedSoFar)`, then goes on to see if the proposed day/time pattern overlaps with another class already placed in the room using `no_overlap()`, which looks like this

```prolog
no_overlap(_,[]).
no_overlap(Proposed,[PlacedHead|PlacedTail]) :- 
	\+ pair_overlap(Proposed,PlacedHead), 
	no_overlap(Proposed,PlacedTail). 
```

`no_overlap` borrows from Prolog's `member` implementation (see rant above), in iterating through a Prolog-list of all classes placed in a given room at this moment. It has two clauses. The first, which is also the termination clause for the recursion, says "any time pattern does not overlap with anything else, if the room is empty."

The second finds the head of the list of classes in a room (i.e. the first element in the list) and sees if it conflicts with the proposed time by calling `pair_overlap`, which checks to see if the start and end times of two events overlap (see below).  (The two events are the proposed time to place a class and just one of the possibly many classes already placed in a room.) If they do not overlap, the tail (list minus the just checked head) is now passed back into `no_overlap` for checking, which is a new (shorter) list, gradually heading toward the empty `[]` list that will satisfy the terminal clause, and stop the recursion. (This is if the list reduction makes it that far; it will if none of the event-pairs tested overlap in time.)

The `pair_overlap` clause looks like this.

```prolog
pair_overlap([DaysA,TimesA],[DaysB,TimesB]) :- 
	intersection(DaysA,DaysB,S), 
	\+ length(S,0), 
	times_overlap(TimesA,TimesB). 
```

We got lucky implementing this one in Prolog. Why? Well in seeing if two classes overlap, first we need to see if they share a common day of the week.  This means looking at class A's of days, say "MWF", and seeing if there's any overlap with class B's days, perhaps "MTR." days. How will one do this? 

In Prolog we were dreading recursively iterating through two lists in order to somehow find matching day characters. As mentioned, this feels like a small project in itself, that would take a whole day to figure out.  As it often does for us in Prolog, this task, so easily done in a procedural language, would likely have killed (or severely delayed) the whole project. 

Luckily Prolog lists can be thought of as sets, and [SWI-Prolog](https://www.swi-prolog.org), which we used here has ready-made predicates for union, intersection, subtract, and subset of lists. Our need was for the intersection, which returns all elements in common between two lists. So `intersection([m,w,f],[w,f],L)` would instantiate `L` to `[w,f]`.  So checking if two classes have any days in common reduces to seeing if the intesection list is of length zero or not.  A non-zero length `\+ length(S,0)` means there are some overlapping days, so now we better go in and check for overlap between the times at which each class is offered. But what if one was using a lesser Prolog without a nice list library?

We were getting more confident in our progress now, as overlapping times would not be hard to do. Here's the code for it:

```prolog
times_overlap(T1,T2) :- 
                        nth1(1,T1,StartH1), nth1(2,T1,StartM1), 
                        nth1(3,T1,EndH1), nth1(4,T1,EndM1), 
                        nth1(1,T2,StartH2), nth1(2,T2,StartM2), 
                        nth1(3,T2,EndH2), nth1(4,T2,EndM2),
                        Start1 is StartH1*60+StartM1, End1 is EndH1*60+EndM1, Start2 is StartH2*60+StartM2, End2 is EndH2*60+EndM2, 
                        Start1 =< End2, End1 >= Start2.
```

The times needing checking for overlap, `T1` and `T2`, each come in as a list like `[8,10,9,00]` meaning a class that meets from 8:10am to 9:00am.  The `nth1` predicates simply extracts the nth (1=first) element of a list. 

So, we pull out the start and end hours and minutes from each time, `T1` and `T2`, convert them all to total minutes into the day, then check for overlap, using the very enjoyable discussion on this topic at [this page](https://stackoverflow.com/questions/325933/determine-whether-two-date-ranges-overlap).  

This utility function did adapt well to Prolog: keep cranking through the sequence of predicates to feed the final decision on overlapping which is the ` Start1 =< End2, End1 >= Start2` logic.  Again, we're lucky to have the `nth1` predicates, because this would have been another mini-, day-long, and potentially show stopping project: write a predicate to return the nth element of a list.

We originally had times in the data like this `time_slot(0,[[m,w,f],[7:10-8:00]])`, and were hoping to have `times_overlap` look like this

```prolog
times_overlap([SH1:SM1-EH1:EM1],[SH2:SM2-EH2:EM2]),
```

but we could not get Prolog to instantiate the variables as needed. It would just kind of run and do nothing. Anyone know why?

Lastly, there's the `all_classes_placed` predicate, that is the key terminator of this overall plan. We want Prolog to keep searching until all classes have been placed in rooms. It looks like this:


```prolog
all_classes_placed :- 
		findall(X,class(X,_,_),AllClasses), 
		findall(Y,room(_,Y,_),PlacedClasses), 
		msort(AllClasses,S), 
		msort(PlacedClasses,S).
```

Again, we got kind of lucky on getting this implemented.  The `findall` predicate returns a list of all possible answers to a query.  In the first `findall`, we get a list of all classes needing placing into `AllClasses`. In the second, a list of all classes found placed in all rooms Prolog has allocated into `PlacedClasses`. We sort both lists and if they are identical, then all classes must have been placed.

In all of these utility functions, we still submit a break-out Python environment would have suited us much better, but I suppose we are getting better at Prolog through it all.

# First run of the program

The complete code is here with 201 classes to place. (The class data was randomly generated and is hypothetical.)
```prolog
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


:- dynamic room/3.

plan :-
        class(ClassNum,_,TimeSlotGroup),
        time_slot(TimeSlotGroup,DaysTimes),
        fits_in_room(RoomNum,DaysTimes),
        \+ room(_,ClassNum,_),
        assert(room(RoomNum,ClassNum,DaysTimes)),
        all_classes_placed,
        listing(room).

plan :- listing(room).
```

If this is saved as `start0.pl`, we can run it using `swipl -f start0.pl -g 'plan, halt.'` We'll get this as the output

```prolog
room(_, 1, [[m, t, w, r], [7, 10, 8, 0]]).
room(_, 2, [[m, t, w, r], [8, 10, 9, 0]]).
room(_, 3, [[m, t, w, r], [9, 10, 10, 0]]).
room(_, 4, [[m, t, w, r], [10, 10, 11, 0]]).
room(_, 5, [[m, t, w, r], [12, 10, 13, 0]]).
room(_, 6, [[m, t, w, r], [13, 10, 14, 0]]).
room(_, 7, [[m, t, w, r], [14, 10, 15, 0]]).
room(_, 8, [[t, r], [15, 10, 16, 30]]).
room(_, 9, [[t, r], [16, 40, 18, 0]]).
room(_, 10, [[t, r], [18, 10, 19, 30]]).
room(_, 11, [[t, r], [19, 40, 21, 0]]).
room(_, 12, [[m, w, f], [11, 10, 12, 0]]).
room(_, 15, [[m, w, f], [15, 10, 16, 0]]).
room(_, 16, [[m, w, f], [16, 10, 17, 0]]).
room(_, 17, [[m, w, f], [17, 10, 18, 0]]).
room(_, 18, [[m, w, f], [18, 10, 19, 0]]).
room(_, 19, [[m, w, f], [19, 10, 20, 0]]).
room(_, 20, [[m, w, f], [20, 10, 21, 0]]).
```

It appears as if Prolog was unable to instantiate any concrete room numbers, but placed 20 classes in a single representative room.  Notice it packed the room farily well from 7:10-21:00 and even used up some T/R days in between all of the MWF and MTWR classes.  Nice job! But still what about some room numbers and placing all classes?  That's where Constraint Logic Programming (CLP) comes in. Get ready for some fun!

# Constraint Logic Programming (CLP)

After watching the [Power of Prolog](https://www.metalevel.at/prolog) videos, we're convinced that Prolog + CLP is what Prolog should have been all along. The addition of CLP is in all in "modern" (or today's) Prologs. None of this appeared in the books and implementations of the '80 and '90s. Interestingly, it is built into [Gnu-Prolog](http://www.gprolog.org), and has been for some time.  Let's see how CLP would help us here.

## Shortcomings of our code

With our code at the moment, Prolog constrained its class placements to a single room. Recall our plan is to have Prolog place classes in rooms, minimizing the number of rooms used when done.  So why doesn't it expand it solution to encompass more rooms and get all classes placed?  Hmmmm.  When we think about it though, what else could it possibly do?

The room number for a class first appears in the line `fits_in_room(RoomNum,DaysTimes)`, where we want to see if a given day/time chosen for a would-be class to place onflicts with what's already placed in room `RoomNum`. We're kind of lucky our code even runs at all, because we never set `RoomNum` to any value at all! 

So Prolog assumes it can take on any value, and assigns it the `_` value. It is kind of telling us: pick any room you want, and here are 20 classes you can place into it using this arrangement.  We'd have to somehow coax Prolog into making up another room, while not ignoring these already placed classes.  Kind of workable, but contrived, and it limits Prolog view of all classes when trying to squeeze them all in.

What about assigning `RoomNum` to some value manually?  Try editing the `plan` goal to read

```prolog
plan :-
        RoomNum is 1,
        class(ClassNum,_,TimeSlotGroup),
        time_slot(TimeSlotGroup,DaysTimes),
        fits_in_room(RoomNum,DaysTimes),
        \+ room(_,ClassNum,_),
        assert(room(RoomNum,ClassNum,DaysTimes)),
        all_classes_placed,
        listing(room).
```

or even

```prolog
plan :-
        random(1,10,RoomNum),
        class(ClassNum,_,TimeSlotGroup),
        time_slot(TimeSlotGroup,DaysTimes),
        fits_in_room(RoomNum,DaysTimes),
        \+ room(_,ClassNum,_),
        assert(room(RoomNum,ClassNum,DaysTimes)),
        all_classes_placed,
        listing(room).
```

You'll see that Prolog now numbers the room, but still contraints its room usage to just a single room.  In the next section, we'll see that instead of defining a room numnber, we can use the CLP add-ons to Prolog to *constrain* (that is, set an acceptable range) for values for `RoomNum`, which will help out a lot.



## A quick review of CLP

We first learned of CLP addons to Prolog by watching the [Power of Prolog](https://www.metalevel.at/prolog) videos, in particular [this one](https://www.metalevel.at/prolog/clpz). There is also a good writeup on it [here](https://www.swi-prolog.org/man/clpfd.html). There isn't a lot written about CLP + Prolog, so you'l have to dig and experiment on your own to make progress with it. (Maybe this repo can even add to the "literature" on it?)

In sum, Constraint Logic Programming CLP is a way of constraining the numerical value a given variable can have in a solution. In CLPD+Prolog, this constraint is enough to allow its search to continue, without knowing a precise value for a given variable.  This idea will hopefully help Prolog to prune a search space, based on such constraints.  

Here, we use CLP(FD), where FD stands for "finite domain." Finite domain here means some data type that has a "finite" feel to it. For computers, this usually means integers.  On a 16-bit machine, the largest integer is 65,535.  The same for 32-bits would be 2,147,483,647 and 64-bits 9,223,372,036,854,775,807. Now the 32 and 64 bit ranges are quite large, but maybe still somewhat searchable given the right guidance.

## Adding CLP to our code

So back to our `RoomNum` problem. Suppose we know we have at most 50 rooms to place rooms in (this is all the rooms our facility has, for example). As our `plan` goal begins, we tell Prolog that `RoomNum` is an open and flexible quantity, but it must only take on values between 1 and 50 (inclusive).  Prolog+CLD means with this constraint alone, Prolog will now be happy to proceed with its solution search, almost as if `RoomNum` was given an exact values.

Let's modify our code and set this constrain like this:

```prolog
plan :-
        RoomNum #>= 1, RoomNum #=< 50,
        class(ClassNum,_,TimeSlotGroup),
        time_slot(TimeSlotGroup,DaysTimes),
        fits_in_room(RoomNum,DaysTimes),
        \+ room(_,ClassNum,_),
        assert(room(RoomNum,ClassNum,DaysTimes)),
        all_classes_placed,
        listing(room).  
```

Here you'll note `#>=` and `#=<` are the CLP replacements for `>=` and `=<`.  In traditional Prolog,  `RoomNum >= 1, RoomNum =< 50,` would immediately fail, and Prolog would be unable to advanced past this line. (Try it. The program will run, but will not place any classes.)

Running the code with the `#>=` and `#=<` operators put back in still will allow the code to run, but gives the same result as above (that is `_` for all room numbers). This is because there are two aspects to Prolog+CLP: 

1. Setting the constraint and 

1. Forcing Prolog to choose values for variables, within the constraints given.  

Forcing values to be chosen is called "labeling" and can be done using the `indomain` or `labeling` predicates.  Here, we'll use `indomain`, which forces incremental choices of a given variable each time Prolog backtracks to it. 

You can test this as follows. Load up SWI-Prolog and type the following:

```prolog
?- use_module(library(clpfd)).
?- RoomNum #> 0, RoomNum #< 50.
```

The output will be

```prolog
RoomNum in 0..50.
```

But if you type this line

```prolog
?- RoomNum #>= 0, RoomNum #=< 50, indomain(RoomNum).
```

You'll see indeed Prolog will begin iterating through the bounds for `RoomNum`. You can also try ` RoomNum #>= 0, RoomNum #=< 50, label([RoomNum]).` where `label` and `labeling` are predicates you can read about [here](https://www.swi-prolog.org/pldoc/man?section=clpfd).

So all we've done by setting the constraint, is to internally let Prolog know that `RoomNum` has to be between 0 and 50.
So modifying our code to:


```prolog
plan :-
        RoomNum #>= 1, RoomNum #=< 50,
        indomain(RoomNum),
        class(ClassNum,_,TimeSlotGroup),
        time_slot(TimeSlotGroup,DaysTimes),
        fits_in_room(RoomNum,DaysTimes),
        \+ room(_,ClassNum,_),
        assert(room(RoomNum,ClassNum,DaysTimes)),
        all_classes_placed,
        listing(room).  
```

will indeed give us some nice results.

## Results at last!

Finally it works! We'll get an output like this:

```prolog
room(1, 1, [[m, t, w, r], [7, 10, 8, 0]]).
room(1, 2, [[m, t, w, r], [8, 10, 9, 0]]).
room(1, 3, [[m, t, w, r], [9, 10, 10, 0]]).
room(1, 4, [[m, t, w, r], [10, 10, 11, 0]]).
room(1, 5, [[m, t, w, r], [12, 10, 13, 0]]).
room(1, 6, [[m, t, w, r], [13, 10, 14, 0]]).
room(1, 7, [[m, t, w, r], [14, 10, 15, 0]]).
room(1, 8, [[t, r], [15, 10, 16, 30]]).
room(1, 9, [[t, r], [16, 40, 18, 0]]).
room(1, 10, [[t, r], [18, 10, 19, 30]]).
room(1, 11, [[t, r], [19, 40, 21, 0]]).
room(1, 12, [[m, w, f], [11, 10, 12, 0]]).
room(1, 15, [[m, w, f], [15, 10, 16, 0]]).
room(1, 16, [[m, w, f], [16, 10, 17, 0]]).
room(1, 17, [[m, w, f], [17, 10, 18, 0]]).
room(1, 18, [[m, w, f], [18, 10, 19, 0]]).
room(1, 19, [[m, w, f], [19, 10, 20, 0]]).
room(1, 20, [[m, w, f], [20, 10, 21, 0]]).
room(2, 13, [[t, r], [7, 40, 9, 0]]).
room(2, 14, [[t, r], [9, 40, 11, 0]]).
room(2, 21, [[m, w, f], [7, 10, 8, 0]]).
room(2, 22, [[m, w, f], [8, 10, 9, 0]]).
room(2, 23, [[m, w, f], [9, 10, 10, 0]]).
room(2, 24, [[m, w, f], [10, 10, 11, 0]]).
room(2, 25, [[m, w, f], [11, 10, 12, 0]]).
room(2, 26, [[m, w, f], [12, 10, 13, 0]]).
room(2, 27, [[m, w, f], [13, 10, 14, 0]]).
room(2, 28, [[t, r], [12, 10, 13, 10]]).
room(2, 29, [[t, r], [13, 40, 15, 0]]).
room(2, 30, [[t, r], [15, 10, 16, 30]]).
room(2, 31, [[t, r], [16, 40, 18, 0]]).
room(2, 32, [[t, r], [18, 10, 19, 30]]).
room(2, 33, [[t, r], [19, 40, 21, 0]]).
room(2, 40, [[m, w, f], [14, 10, 15, 0]]).
room(2, 46, [[m, w, f], [15, 10, 16, 0]]).
room(2, 47, [[m, w, f], [16, 10, 17, 0]]).
room(2, 48, [[m, w, f], [17, 10, 18, 0]]).
room(2, 49, [[m, w, f], [18, 10, 19, 0]]).
room(2, 50, [[m, w, f], [19, 10, 20, 0]]).
room(2, 51, [[m, w, f], [20, 10, 21, 0]]).
room(3, 34, [[t, r], [7, 40, 9, 0]]).
room(3, 35, [[t, r], [9, 40, 11, 0]]).
room(3, 36, [[t, r], [12, 10, 13, 10]]).
room(3, 37, [[t, r], [13, 40, 15, 0]]).
room(3, 38, [[t, r], [15, 10, 16, 30]]).
room(3, 39, [[t, r], [16, 40, 18, 0]]).
room(3, 41, [[t, r], [18, 10, 19, 30]]).
room(3, 42, [[t, r], [19, 40, 21, 0]]).
room(3, 52, [[m, w, f], [7, 10, 8, 0]]).
room(3, 53, [[m, w, f], [8, 10, 9, 0]]).
room(3, 64, [[m, w, f], [9, 10, 10, 0]]).
room(3, 65, [[m, w, f], [10, 10, 11, 0]]).
room(3, 66, [[m, w, f], [11, 10, 12, 0]]).
room(3, 67, [[m, w, f], [12, 10, 13, 0]]).
room(3, 68, [[m, w, f], [13, 10, 14, 0]]).
room(3, 69, [[m, w, f], [14, 10, 15, 0]]).
room(3, 70, [[m, w, f], [15, 10, 16, 0]]).
room(3, 71, [[m, w, f], [16, 10, 17, 0]]).
room(3, 86, [[m, w, f], [17, 10, 18, 0]]).
room(3, 87, [[m, w, f], [18, 10, 19, 0]]).
room(3, 92, [[m, w, f], [19, 10, 20, 0]]).
room(3, 100, [[m, w, f], [20, 10, 21, 0]]).
room(4, 43, [[t, r], [7, 40, 9, 0]]).
room(4, 44, [[t, r], [9, 40, 11, 0]]).
room(4, 45, [[t, r], [12, 10, 13, 10]]).
room(4, 54, [[t, r], [13, 40, 15, 0]]).
room(4, 55, [[t, r], [15, 10, 16, 30]]).
room(4, 56, [[t, r], [16, 40, 18, 0]]).
room(4, 57, [[t, r], [18, 10, 19, 30]]).
room(4, 58, [[t, r], [19, 40, 21, 0]]).
room(4, 101, [[m, w, f], [7, 10, 8, 0]]).
room(4, 102, [[m, w, f], [8, 10, 9, 0]]).
room(4, 103, [[m, w, f], [9, 10, 10, 0]]).
room(4, 104, [[m, w, f], [10, 10, 11, 0]]).
room(4, 105, [[m, w, f], [11, 10, 12, 0]]).
room(4, 106, [[m, w, f], [12, 10, 13, 0]]).
room(4, 107, [[m, w, f], [13, 10, 14, 0]]).
room(4, 108, [[m, w, f], [14, 10, 15, 0]]).
room(4, 109, [[m, w, f], [15, 10, 16, 0]]).
room(4, 114, [[m, w, f], [16, 10, 17, 0]]).
room(4, 115, [[m, w, f], [17, 10, 18, 0]]).
room(4, 116, [[m, w, f], [18, 10, 19, 0]]).
room(4, 117, [[m, w, f], [19, 10, 20, 0]]).
room(4, 118, [[m, w, f], [20, 10, 21, 0]]).
room(5, 59, [[t, r], [7, 40, 9, 0]]).
room(5, 60, [[t, r], [9, 40, 11, 0]]).
room(5, 61, [[t, r], [12, 10, 13, 10]]).
room(5, 62, [[t, r], [13, 40, 15, 0]]).
room(5, 63, [[t, r], [15, 10, 16, 30]]).
room(5, 72, [[m, t, w, r], [17, 10, 18, 0]]).
room(5, 73, [[m, t, w, r], [18, 10, 19, 0]]).
room(5, 74, [[m, t, w, r], [19, 10, 20, 0]]).
room(5, 75, [[m, t, w, r], [20, 10, 21, 0]]).
room(5, 119, [[m, w, f], [7, 10, 8, 0]]).
room(5, 120, [[m, w, f], [8, 10, 9, 0]]).
room(5, 121, [[m, w, f], [9, 10, 10, 0]]).
room(5, 123, [[m, w, f], [10, 10, 11, 0]]).
room(5, 124, [[m, w, f], [11, 10, 12, 0]]).
room(5, 125, [[m, w, f], [12, 10, 13, 0]]).
room(5, 126, [[m, w, f], [13, 10, 14, 0]]).
room(5, 127, [[m, w, f], [14, 10, 15, 0]]).
room(5, 132, [[m, w, f], [15, 10, 16, 0]]).
room(5, 133, [[m, w, f], [16, 10, 17, 0]]).
room(6, 76, [[m, t, w, r], [7, 10, 8, 0]]).
room(6, 77, [[m, t, w, r], [8, 10, 9, 0]]).
room(6, 78, [[m, t, w, r], [9, 10, 10, 0]]).
room(6, 79, [[m, t, w, r], [10, 10, 11, 0]]).
room(6, 80, [[m, t, w, r], [12, 10, 13, 0]]).
room(6, 81, [[m, t, w, r], [13, 10, 14, 0]]).
room(6, 82, [[t, r], [15, 10, 16, 30]]).
room(6, 83, [[t, r], [16, 40, 18, 0]]).
room(6, 84, [[t, r], [18, 10, 19, 30]]).
room(6, 85, [[t, r], [19, 40, 21, 0]]).
room(6, 89, [[m, t, w, r], [14, 10, 15, 0]]).
room(6, 134, [[m, w, f], [11, 10, 12, 0]]).
room(6, 148, [[m, w, f], [15, 10, 16, 0]]).
room(6, 149, [[m, w, f], [16, 10, 17, 0]]).
room(6, 150, [[m, w, f], [17, 10, 18, 0]]).
room(6, 151, [[m, w, f], [18, 10, 19, 0]]).
room(6, 156, [[m, w, f], [19, 10, 20, 0]]).
room(6, 168, [[m, w, f], [20, 10, 21, 0]]).
room(7, 88, [[t, r], [7, 40, 9, 0]]).
room(7, 90, [[m, t, w, r], [9, 10, 10, 0]]).
room(7, 91, [[m, t, w, r], [10, 10, 11, 0]]).
room(7, 93, [[t, r], [12, 10, 13, 10]]).
room(7, 94, [[t, r], [13, 40, 15, 0]]).
room(7, 95, [[t, r], [15, 10, 16, 30]]).
room(7, 96, [[t, r], [16, 40, 18, 0]]).
room(7, 97, [[t, r], [18, 10, 19, 30]]).
room(7, 98, [[t, r], [19, 40, 21, 0]]).
room(7, 169, [[m, w, f], [7, 10, 8, 0]]).
room(7, 170, [[m, w, f], [8, 10, 9, 0]]).
room(7, 171, [[m, w, f], [11, 10, 12, 0]]).
room(7, 178, [[m, w, f], [12, 10, 13, 0]]).
room(7, 179, [[m, w, f], [13, 10, 14, 0]]).
room(7, 180, [[m, w, f], [14, 10, 15, 0]]).
room(7, 181, [[m, w, f], [15, 10, 16, 0]]).
room(7, 182, [[m, w, f], [16, 10, 17, 0]]).
room(7, 183, [[m, w, f], [17, 10, 18, 0]]).
room(7, 184, [[m, w, f], [18, 10, 19, 0]]).
room(7, 185, [[m, w, f], [19, 10, 20, 0]]).
room(7, 186, [[m, w, f], [20, 10, 21, 0]]).
room(8, 99, [[t, r], [7, 40, 9, 0]]).
room(8, 110, [[t, r], [9, 40, 11, 0]]).
room(8, 111, [[t, r], [12, 10, 13, 10]]).
room(8, 112, [[t, r], [13, 40, 15, 0]]).
room(8, 113, [[t, r], [15, 10, 16, 30]]).
room(8, 122, [[t, r], [16, 40, 18, 0]]).
room(8, 128, [[t, r], [18, 10, 19, 30]]).
room(8, 129, [[t, r], [19, 40, 21, 0]]).
room(8, 187, [[m, w, f], [7, 10, 8, 0]]).
room(8, 188, [[m, w, f], [8, 10, 9, 0]]).
room(8, 193, [[m, w, f], [9, 10, 10, 0]]).
room(8, 194, [[m, w, f], [10, 10, 11, 0]]).
room(8, 195, [[m, w, f], [11, 10, 12, 0]]).
room(9, 130, [[t, r], [7, 40, 9, 0]]).
room(9, 131, [[t, r], [9, 40, 11, 0]]).
room(9, 135, [[t, r], [12, 10, 13, 10]]).
room(9, 136, [[t, r], [13, 40, 15, 0]]).
room(9, 137, [[t, r], [15, 10, 16, 30]]).
room(9, 138, [[t, r], [16, 40, 18, 0]]).
room(9, 139, [[t, r], [18, 10, 19, 30]]).
room(9, 140, [[t, r], [19, 40, 21, 0]]).
room(10, 141, [[t, r], [7, 40, 9, 0]]).
room(10, 142, [[t, r], [9, 40, 11, 0]]).
room(10, 143, [[t, r], [12, 10, 13, 10]]).
room(10, 144, [[t, r], [13, 40, 15, 0]]).
room(10, 145, [[t, r], [15, 10, 16, 30]]).
room(10, 146, [[t, r], [16, 40, 18, 0]]).
room(10, 147, [[t, r], [18, 10, 19, 30]]).
room(10, 152, [[t, r], [19, 40, 21, 0]]).
room(11, 153, [[t, r], [7, 40, 9, 0]]).
room(11, 154, [[t, r], [9, 40, 11, 0]]).
room(11, 155, [[t, r], [12, 10, 13, 10]]).
room(11, 157, [[t, r], [13, 40, 15, 0]]).
room(11, 158, [[t, r], [15, 10, 16, 30]]).
room(11, 159, [[t, r], [16, 40, 18, 0]]).
room(11, 160, [[t, r], [18, 10, 19, 30]]).
room(11, 161, [[t, r], [19, 40, 21, 0]]).
room(12, 162, [[t, r], [7, 40, 9, 0]]).
room(12, 163, [[t, r], [9, 40, 11, 0]]).
room(12, 164, [[t, r], [12, 10, 13, 10]]).
room(12, 165, [[t, r], [13, 40, 15, 0]]).
room(12, 166, [[t, r], [15, 10, 16, 30]]).
room(12, 167, [[t, r], [16, 40, 18, 0]]).
room(12, 172, [[t, r], [18, 10, 19, 30]]).
room(12, 173, [[t, r], [19, 40, 21, 0]]).
room(13, 174, [[t, r], [7, 40, 9, 0]]).
room(13, 175, [[t, r], [9, 40, 11, 0]]).
room(13, 176, [[t, r], [12, 10, 13, 10]]).
room(13, 177, [[t, r], [13, 40, 15, 0]]).
room(13, 189, [[t, r], [15, 10, 16, 30]]).
room(13, 190, [[t, r], [16, 40, 18, 0]]).
room(13, 191, [[t, r], [18, 10, 19, 30]]).
room(13, 192, [[t, r], [19, 40, 21, 0]]).
room(14, 196, [[t, r], [7, 40, 9, 0]]).
room(14, 197, [[t, r], [9, 40, 11, 0]]).
room(14, 198, [[t, r], [12, 10, 13, 10]]).
room(14, 199, [[t, r], [13, 40, 15, 0]]).
room(14, 200, [[t, r], [15, 10, 16, 30]]).
room(14, 201, [[t, r], [16, 40, 18, 0]]).
```



Here you'll see that all 201 classes have been placed, and we'll need 14 rooms. We can also see that the actual search "did something," since the placement of classes is not strictly sequential. The class packing is quite dense, as can be shown here (in a visiualizer written in Javascript).

Rooms 1-2 looks like this, pretty densely packed, as we hoped.

![Rooms 1-2](https://github.com/tbensky/PrologScheduling/blob/master/Results/rooms1-2.png)

Starting at about Room 9, the density drops since the T/TH classes become all that's left to place. Many such rooms look like this:

![Rooms13-14](https://github.com/tbensky/PrologScheduling/blob/master/Results/rooms13-14.png)


# What about minimizing the rooms used?

For the 201 classes used in this run, we are pleased with the room packing, and indeed 14 rooms seems reasonable.  A lingering question: Where in the code did we tell Prolog to minimize the rooms it should use?

We didn't, because it seems to be doing this already, or our scheduling problem isn't as complicated as it seems.  When we look at the rigid structure of our classes, with the strict MWF or T/TH offerings, maybe it's not hard to pack classes in. (?)  The results shown above do not show any wasteful room usage. There are more time patterns (MW and one day/week classes) which aren't used as much, but may complicate things.

Specifying the desire to minimize `RoomNum` (we think) is in allowing Prolog to choose a room number value for `RoomNum`. The `indomain` predicate incrementally goes through the domain of `RoomNum`. There is another way of instantiating `RoomNum` using the `labeling` predicate like this:

```prolog
labeling([min(RoomNum)],[RoomNum]))
```

This tells Prolog to find a value for `RoomNum` while working to minimize it. There are several options for `labeling` in that first parameter (including for example, maximizing a variable).

If we change our core code to be

```prolog
plan :-
        RoomNum #>= 1, RoomNum #=< 50,
        labeling([min(RoomNum)],[RoomNum])),
        class(ClassNum,_,TimeSlotGroup),
        time_slot(TimeSlotGroup,DaysTimes),
        fits_in_room(RoomNum,DaysTimes),
        \+ room(_,ClassNum,_),
        assert(room(RoomNum,ClassNum,DaysTimes)),
        all_classes_placed,
        listing(room).
```

We get the same result (14 rooms), but again, maybe `indomain` is sufficient for our particular data set.


# What now?

So we developed a Prolog+CLP-based room scheduler. Here are a few closing thoughts.

## More constraints

Suppose we need a given class to be placed in a particular room. Maybe room #3 is a laboratory, so we need laboratory-based classes placed in there. Turns out something like this is farily straightfoward to add. Maybe room #7 has theater-like seating for lecture-only classes.

First, put some `must_place` predicates under the `class` definitions, like this


```prolog
must_place_in(1,3).
must_place_in(201,7).
must_place_in(_,_) :- fail.
```

This means class 1 should be placed into room 3 and class 201 into room 7.  We'll explain the `must_place_in(_,_) :- fail` below.

Next modify the `indomain` line to read as shown here.

```prolog
plan :-
        RoomNum #>= 1, RoomNum #=< 50,
        (must_place_in(ClassNum,RoomNum)  ; indomain(RoomNum)),
        class(ClassNum,_,TimeSlotGroup),
        time_slot(TimeSlotGroup,DaysTimes),
        fits_in_room(RoomNum,DaysTimes),
        \+ room(_,ClassNum,_),
        assert(room(RoomNum,ClassNum,DaysTimes)),
        all_classes_placed,
        listing(room).
```

This works as follows, focusing on the line

```
 (must_place_in(ClassNum,RoomNum)  ; indomain(RoomNum)),
 ```

Note the outer `(` and `)` are very important here.

 When trying to force Prolog to choose a value for `RoomNum`, we allow it to do so in one of two ways. First, we see if a constraint via the `must_place_in` predicate exists for a given `ClassNum`. If so, instantiate `RoomNum` to the required value. If `must_in_place` fails, as in, "class `ClassNum` has no fixed room requirement," then we defer to `indomain` to assign a value to `RoomNum`.  This is why we need the `must_place_in(_,_) :- fail.` definition, since we need `must_in_place` to explicitly fail to trigger `indomain`, if no explicit class room requirement is given. Running this indeed places the rooms as needed:

 ```prolog
room(3, 1, [[m, t, w, r], [7, 10, 8, 0]]).
room(7, 201, [[t, r], [7, 40, 9, 0]]).
room(1, 2, [[m, t, w, r], [7, 10, 8, 0]]).
room(1, 3, [[m, t, w, r], [8, 10, 9, 0]]).
room(1, 4, [[m, t, w, r], [9, 10, 10, 0]]).
room(1, 5, [[m, t, w, r], [10, 10, 11, 0]]).
room(1, 6, [[m, t, w, r], [12, 10, 13, 0]]).
room(1, 7, [[m, t, w, r], [13, 10, 14, 0]]
	...
	...
```

## More constraints

More scheduling constraints are possible. Some we haven't explored yet:

* Being sure two or more classes are offered in sequence on a particular day.

* Being sure some classes are not offered at the same time (no matter the room).

* Linking the offerings times of 3 classes. For example, a lecture class that also have labs associated with it.  We might want the labs occur on off-days for the lecture.

We're sure there are scheduling constraint out there.



## Enumerating your plan

With CLP, it becomes important to phrase your logic in terms of integers.  In this case, we used room numbers as integers to work with CLP, that can then be mapped to actual rooms with actual locations later.  

Another parameter worth enumerating here may be the time of day chosen for a class, in a given time slot group (meaning the `DaysTimes` variable). This would be constrained between the total possible time slots (14 for time slot 0). This means both the room number and `DaysTimes` would be undetermined variables open for optimization by the search. 

This may sound a bit circular, but `DaysTimes` is now chosen by rote data pulling via the line `time_slot(TimeSlotGroup,DaysTimes)`, which may be somewhat constraining to the search.  We note in particular when using the `must_place` constraint how the affected classes are placed very first, at even at the first possible time slot allowed for that class. This may not be optimal for overall room packing for the schedule as a whole, but may be if `DaysTimes` was enumerated, and constrained as suggested here.   We have not tried this yet.
