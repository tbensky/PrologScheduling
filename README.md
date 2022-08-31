# Scheduling Prolog using Constraint logic

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

# Prolog and Scheduling: getting started


In sum, here's how this scheduling project works.  

I have N classrooms and M classes to be placed within the rooms.  M can vary, and I'd like to
pack the classes into a mininmal number of rooms. In other words, pack in the classes, minimizing N.  

The classes are specified (by me), as having a name, number, and
required time slot. The time slot comes from a list of times that a class is allowed to be scheduled over. For example, one group of
time slots (group 0, just for labeling sense) might be:

```
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

This means any class that is needed to be schedule as a "group 0" class will need to be placed on Mon, Wed, or Fri, from 7:10-8am, 8:10-9am, etc.
By the way, this is actual Prolog code. See how easy "the data" can be?

