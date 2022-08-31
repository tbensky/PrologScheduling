# Scheduling Prolog using Constraint Logic Programming (CLP)

This is a simple, gentle, and step-by-step example of how to use
Prolog and constraint logic programmig (CLP) to schedule events
(in this case college classes), into a minimum number of rooms.

## First a few thoughts

I find Prolog to be an extremely satisfying language to use. I first
dabbled with it back in '85 when Borland came out with Turbo Prolog 1
and then 2. I owned them both.  The manuals were great tutorials on
Prolog (recently bought them all again on eBay). Kudos to the
knowledgable people at Borland who wrote these. I look at Prolog on
and off, sometimes with a fair amount of intensity for a few weeks,
then I set it down again for a while. I've been doing with for the
better part of 35 years! Ouch! This repo actually contains the very
first Prolog program I wrote from scratch, to solve a scheduling
problem I had, and I actually use it! I don't mean this in a
historical sense. After all of these years, this is actually the
first Prolog program I've written just recently (Aug 2022), during
another one of my "let's learn Prolog phases.")


Whenever I'm able to get it to do something that I need, or have
some "ah ha that's how that works" moment with it, I find it very
rewarding, like I've actually just learned something.  Prolog
however, has an odd reputation out there (which is also how I used
to think about it).

I've read many Stackoverflow questions where someone will say
something like "I need to automate some scheduling" or "I want to add
NLP to my application," or "I need to do X," (that is has some AIish
or 'hard' problem, without an obvious procedural language solution).
They'll then go on to say "I heard Prolog is really good at this,"
and will post some code like `class(intro_to_coding,T,1:30-3:30).` or
`is_verb(walk).` and throw their hands in the air with a closing
thought like "What do I do now? I thought Prolog is supposed to know
how to handle this."

This is not the case. All told, Prolog is a search language. It's core
contains all of the best practices for searching data you give it,
against some pattern you are looking for (that you also give it). It is generalized
search language, that uses backtracking for search efficiency.  

The power of Prolog as I see it is twofold:

1. You don't have to go writing the search algorithms yourself, and 

2. Prolog makes specifying your data and pattern(s) very natural, maybe even
easier than with a procedural language. Both are just parts of
the actual Prolog code. I find specifying the data usually doable (and even enjoyable). The pattern,
which will drive the search, much less so.

I think people get hung up on the pattern they are looking for, and
kind of expect Prolog to just "know" such things. In scheduling for
example, there is no built in magic of Prolog like `make_a_schedule(X),` 
where you hit return and X will be a list of rooms and what
classes can be placed into it.  You still have to code up the Prolog
that ensures classes aren't placed on top of each other, some kind of
termination condition, and some data structure that even models
a "room full of classes."

As Triska pointed out, if you've wandered over to Prolog, you probably
have a difficult problem to solve. You're looking at Prolog because
it is a difficult problem, and would also be made even more so if
solved in a procedural language.  Prolog may make the implementation
easier on you.

# Prolog and Scheduling: Getting Started

## What scheduling I need

I have `N` classrooms and `M` classes to be placed within the rooms.  The number of classes, `M`, can vary, and I'd like to
pack the classes into a mininmal number of rooms. In other words, pack in the `M` classes, minimizing `N`.  

The classes are specified (by me), as having a name, number, and
required time slot. The time slot comes from a list of times that a class is allowed to be scheduled over. For example, one group of
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

This means any class that is needed to be schedule as a `group 0` class will need to be placed on Mon, Wed, or Fri, from 7:10-8am, 8:10-9am, etc. In other
words, although a `group 0` class  is always 50 min long, it cannot just be placed at just any time, like 10:15am-11:05am. 

Here are some more time slots for the `group 1` Tues/Thurs (t=tues, r=thurs) classes:


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

Here are a list of how classes are specified.

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

So we have a few classes (numbered 1-13), with some name, and needing to be placed at some time that is consistent with a time slot group. Here, `geol-1200-01` needs to be placed on Tues/Thurs
according to a time in the group 1 time slots.  Class #6, or `geol-1206-1` shall be placed as a group 0 class.

I feel like just which time to choose for a class, within a timeslot group, should be left up to Prolog. Thus, no more specifics on how to place a class will be given.  Many might think trying a random time within a group of time slots is the way to go.  Maybe it is, maybe it isn't. Let's just let Prolog grapple with this.


Believe it or not, as complicated as "computer automated scheduling" may sound, that's it for our application.  I tell Prolog about valid time slots, and classes. From this alone, I want it to place the `M` classes (13 here), into `N` rooms, where `N` is a minimum.  In practice, this is run for `M=200+` classes, simply by extending the `class()` data set.  We also have about 6 or 7 time slot groups. 

By the way, these blocks are actual Prolog code. See how easy "the data" can be?


 # Thinking it through

 Here's where the Stackoverflow questions on Prolog I alluded to above tend to break down: Given the data above, doesn't Prolog somehow just know what to do? No, so let's think it through. 

 Given the data, what would we envision would be a successful scheduling algorithm? Here are some key goals:

 * Get a class to be placed.

 * Get its required time slot group.

 * Place the class in some room, and when placing it, be sure it doesn't conflict (or overlap) with another class already in the room.

 * Only place a class once (i.e. don't place the same class in more than 1 room).

 * Keep doing this until all classes placed into a room.


 This is all I can think of for this too. Seems like if all of the above is done, we just might have our rooms scheduled!

## Main goal

As you know, Prolog has goals you tell it to try and resolve with data.  Let's translate the above steps into Prolog:

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

1. Get a class from the data using `class(ClassNum,_,TimeSlotGroup)`. `ClassNum` will be the class's number (for easy reference), and `TimeSlotGroup` will its required time slot group. This is all just rote data at this point. All we care about at the moment is the class number and what time slot group it requires. So we ignore the class name right now with the `_`. We could do all of this using a class's name, but for now we'll just stick with its number.

1. Ok, so given the class requires `TimeSlotGroup`, lets use ` time_slot(TimeSlotGroup,DaysTimes),` to grab some proposed (and specific) days and times (`DaysTimes`) from that time slot group. We don't know which `DaysTimes` Prolog will choose, but we'll leave that up to it (not us).

1. So we have a class, a proposed day/time pattern and are prepared to place it into a room, whose number is `RoomNum.` Before placing it, let's be sure it doesn't conflict with a class that may already be in the room using `fits_in_room(RoomNum,DaysTimes)`.

1. We use Prolog's built in database `assert()` to keep track of placed classes. But before we assert a class into the database as being "placed," let's be sure we don't do so if it has already been placed using `\+ room(_,ClassNum,_)`. 

1. If it hasn't been placed, we'll go ahead and place it using `assert(room(RoomNum,ClassNum,DaysTimes))`.

1. Next we check if all classes have been placed using `all_classes_placed`.

1. If so, we list out the room assertions using `listing(room)`, so the user can see the result.

The `:- dynamic room/3.` is a Prolog directive, telling it as the assertions of `room` (with 3 parameters) into the database is done at runtime (dynamically); `room` does not have any static representations in static data.

## The utility routines

We can't run our code just yet, as some calls are not defined, namely `fits_in_room`, and `all_classes_placed`. So we need to develop these functions before the code will run. A few thoughts on this.

### Thoughts

Many times in our study of Prolog, utility routines is where we would usually bail out and abandon our Prolog project. Why? Because usually the utililty routines are 

1. Boring.

1. Easy solved by a procedural language.

1. More difficult to solve in Prolog.

1. Not impressive or sexy, when we finally figure out how to grind them out in Prolog (not AIish, not Prology, nothing special). As a case in point, look at Quicksort implemented in Prolog. All we can say is "why" as in post it here (https://www.reddit.com/r/DiWHY/).

More than once, we've wished Prolog would have some kind of embedded Python within it, that easily took and returned Prolog's native data types for some quick processing. Thus, quick procedural needs can be taken care of quickly.

We, for example, don't really like recursion. We'd much rather iterate through a Prolog list (for example) with a for-loop, and have tons of the usual C/PHP/Python-type string functions available (trim, strupper, etc.) to us when doing so. All to be passed  neatly back into a Prolog datatype so it may continue.

###

Here's our first needed utility routine, that we would have rather handled procedurally, but by some miracle, we were able to grid out in Prolog: `fits_in_room`, which sees if a proposed day/time pattern called `DaysTimes` fits in room `RoomNum`.  Here's out definition for it:

`fits_in_room(RoomNum,ProposedDaysTimes) :- findall(X,room(RoomNum,_,X),PlacedSoFar), no_overlap(ProposedDaysTimes,PlacedSoFar).`

It takes in some `RoomNum` and propopsed time pattern `ProposedDaysTimes`.  It generates a list of all classes placed thus far in room `RoomNum` using `findall(X,room(RoomNum,_,X),PlacedSoFar)`, then goes on to see if the proposed day/time pattern overlaps with another class already placed in the room.

