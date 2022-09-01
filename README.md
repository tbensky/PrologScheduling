# Solving a scheduling problem using Prolog using Constraint Logic Programming (CLP)

This is a simple, gentle, and step-by-step example of how to use
Prolog and constraint logic programmig (CLP) to schedule events
(in this case college classes), into a minimum number of rooms.

## Funny realization

I've been dabbling with Prolog every since high school. That was 30+ years
ago. In all of this time, this is the first Prolog program I've ever written myself, that actually
does something useful.  (You can see some results of my dabbling [here](https://www.codebymath.com/index.php/welcome/lesson_menu#prolog) and [here](https://arxiv.org/abs/2108.09893).)

## First a few thoughts

I find Prolog to be an extremely satisfying language to use and mess around with. I first
became aware of it with it back in '85 when Borland came out with Turbo Prolog 1
and then 2. I owned them both. The Borland manuals were great tutorials on
Prolog (recently bought them all again on eBay). Kudos to the
knowledgable people at Borland who wrote them. 

I look at Prolog on
and off, sometimes with a fair amount of intensity for a few weeks,
then I set it down again for a while. I've been doing with for the
better part of 30 years! Ouch! This repo actually contains the very
first Prolog program I wrote from scratch, to solve ongoing scheduling
work I do. I actually use it, and I don't mean this in a
historical sense. After all of these years, this is actually the
first Prolog program I've written (in Aug 2022), during
another one of my "let's try Prolog" phases.


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
Prolog is a just search language. It's core contains all of the best practices
for matching data (you give it), to some pattern you are looking for (that you
also give it). It returns
"true" if it finds a match between the two.  It is generalized search language,
 that uses backtracking for search efficiency.  

The power of Prolog as we see it is twofold:

1. You don't have to go writing the search algorithms yourself. Prolog's are all professional-grade, so you can consider tackling "large" search spaces. 

2. Prolog makes specifying your data and pattern(s) very natural, maybe even
easier than with a procedural language. Both are just parts of
the actual Prolog code. I find specifying the data usually very enjoyable. The pattern,
which will drive the search, much less so however.

I think with Prolog, people get hung up on stating the pattern they are looking for in their data. They 
kind of expect Prolog to just "know" such things. 

In scheduling for
example, there is no built in magic of Prolog like `make_a_schedule(X),` 
where you hit return and X will be a list of rooms and what
classes can be placed into it.  You still have to code up the Prolog
that ensures classes aren't placed on top of each other, some kind of
termination condition, some data structure that even models
a "room full of classes," etc.

As Triska pointed out, if you've wandered over to Prolog, it's probably because you
have a difficult problem to solve. Likely with a large phase-space to explore.
 You're looking at Prolog because even at the start, you are already stuck on what to do. You maybe don't
 see an easy route in a procedural language.  

Prolog, at minimum, makes searching the large
space plausible, and may even make the implementation easier on you. In your procedural
language, all you can think of is `for(i=0;i<1000000000;i++)`, which you already know won't really work.

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


### Hmmm....scheduling these classes

As a scheduling plan goes, we feel like just which time to choose for a class, within a timeslot group, should be left up to Prolog. Thus, no more specifics on when to place a class will be given.  Many might think trying a random time within a group of time slots is the way to go.  Maybe it is, maybe it isn't. It's not here. (But a random number-based algorithm will place classes in rooms for you, but it has a funny convergence and completion problem.)  Let's just let Prolog grapple with this parameter.


Believe it or not, as complicated as "computer automated scheduling" may sound, that's it for our application.  We tell Prolog about valid time slots, and classes needing to be placed. From this alone, we want Prolog to place the `M` classes (13 here), into `N` rooms, where `N` is a minimum.  In practice, this is run for `M=200+` classes, simply by extending the `class()` data set.  We also have about 6 or 7 actual time slot groups. 

By the way, these blocks are actual Prolog code. See how easy "the data" can be?


 # Thinking it through

 Here's where the Stackoverflow questions on Prolog alluded to above tend to break down: Given the data above, doesn't Prolog somehow just know what to do? No it does not.  At minimum some "cost" function needs to be specified so any algorithm knows how it's doing (see Lex/Norvig interview).  How would Prolog know your cost function for your hard problem? Let's think it through. 

Given the data, what would we envision of a successful scheduling algorithm? Here are some key goals:

For each class to be placed:

 * Get its required time slot group.

 * Place it in some room, and when placing doing so, be sure it doesn't conflict (or overlap) with another class already in the room.

 * Only place it once.

 Keep doing this until all classes placed into a room. Expand needed rooms, but being careful not to be wasteful with them (i.e. one class per room would work, but....).


 This is all we can think of for a scheduling algorithm. Seems like if all of the above is done, we just might have our rooms scheduled! A beauty of
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

We got lucky implementing this one in Prolog. Why? Well in seeing if two classes overlap, first you need to see if they share a common day.  This means looking at class A's days of "MWF" and seeing if there's any overlap with class B's "MTR" days. How will one do this?






