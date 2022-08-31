# Scheduling Prolog using Constraint logic

This is a simple, gentle, and step-by-step example of how to use Prolog
and constraint logic programmig (CLP) to schedule events(in this case
college classes) into a minimum number of rooms.

## First a few thoughts

I find Prolog to be an extremely satisfying language to use. I first
dabbled with it back in '85 when Borland came out with Turbo Prolog 1
and then 2. I owned them both.  The manuals were great tutorials on
Prolog (recently bought them all again on eBay). Kudos to the
knowledgable people at Borland who wrote those. I look at Prolog on and
off, sometimes with a fair amount of intensity for a few weeks, then I
set it down again for a while. I've been doing with for the better part
of 35 years! Ouch! This repo actually contains the very first Prolog
program I wrote from scratch, to solve a scheduling problem I had, and
I actually use it!


Whenever I'm able to get it to do something that I need, or have
some "ah ha that how it works" moment with it, I find it very
rewarding, like I've actually just learned something.  Prolog however,
has an odd reputation out there.

I've read many Stackoverflow questions where someone will say something
like "I need to automate some scheduling" or "I want to add NLP to my
application," or "I need to do X," (that is has some AIish or 'hard'
feel, without an obvious procedural language solution).  They'll then
go on to say "I heard Prolog is really good at this," and will post
some code like `class(intro_to_coding,T,1:30-3:30).` or `is_verb
(walk).` and throw their hands in the air with a closing thought
like "What do I do now? I thought Prolog is supposed to know how to
handle this."

This is not the case. Prolog, all told is a search language. It's core
contains all of the best practices for searching data you give it, for
some pattern you are looking for (that you also give it). The power of
Prolog as I see it is twofold:

1. You don't have to go writing the search algorithms yourself, and 2.
Prolog makes supplying the data very natural and easy.  The pattern you
need somewhat easy, but not entirely so.  Both are just parts of the
actual Prolog code.

I think people get hung up on the pattern they are looking for, and kind
of expect Prolog to just "know" such things. In scheduling for example,
there is no built in magic of Prolog like `make_a_schedule(X),` where
you hit return and X will be a list of rooms and what classes can be
placed into it.  You still have to code up the Prolog that ensures
classes aren't placed on top of each other, some kind of termination
condition, and some data structure that even models a "room full of
classes."

As Triska pointed out, if you've wandered over to Prolog, you probably
have a difficult problem to solve. You're looking at Prolog because
this is a difficult problem, and would also be made even more so if
solved in a procedural language.  Prolog may make the implementation
easier on you.

# Prolog and Scheduling: getting started