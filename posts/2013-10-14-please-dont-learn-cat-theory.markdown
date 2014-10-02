-----
title: Please Don't Learn Category Theory
tags: haskell, opinions
-----

I spend a lot of time talking about Haskell. Trying to get imperative programmers to learn Haskell is hard,
people are stubborn and often have misconceptions about functional programming.

One of the most common excuses I hear is

> **I don't know enough math to learn Haskell, don't you need to know category theory?**

No! In fact, please don't go learn category theory to learn Haskell. Why not?

The Haskell standard library makes shallow use of quite deep and complex ideas.
Yes it uses the words Monad, Functor, and Category. But you don't need to have any
idea what these words mean to use them. In fact, try substituting

     Monad    -> FuzzyWuzzy
     Functor  -> Banana
     Category -> Cheerios

And you'll still be able to learn/use Haskell just fine. In fact, there's a lovely series of
[problems](http://blog.tmorris.net/posts/20-intermediate-haskell-exercises/) that do just this.


> **But wait, why did they even bother with the names then?**

Because that's where the idea comes from. The abstractions in Haskell are sometimes inspired
by math. There's no argument there. And the people who designed Haskell decided they weren't going
to pretend they didn't use math. But just like how Ruby was inspired by Lisp and Smalltalk,
you don't need to learn the source of some abstraction to enjoy it.

In fact, if you want to see a bunch of category theory inspired abstractions, check out
some of Edward Kmett's libraries. They're just littered with big-n-scary words. However, you
can still use `lens` (a super useful library) without understanding what it means to
"downstar a functor into a profunctor". And tons do.

> **Is there any point in learning category theory then?**

I'd say so, it's a cool piece of mathematics to start with. And who knows, plenty of
people find it useful to look to category theory for reasoning about abstractions.

I decided to pick up a few books in June and have been thoroughly enjoying it. Not
because it suddenly made me better at Haskell but because I like math and it provides
a good language for talking about some concepts.

Who knows, maybe you're a budding category theorist.

> **What do I need to know before Haskell then?**

Well um, not much. In fact, the less you know the better! I came to Haskell
from primarily imperative languages (C, Perl, Java) and the biggest problem I had
was trying not to write Perl in Haskell.

The only thing I found terribly helpful was already understanding what a
type was. And I think that can be picked up pretty quickly.

Good luck :)
