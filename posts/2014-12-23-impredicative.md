---
title: What Are Impredicative Types?
tags: haskell, types
---

So the results from Stephen's [poll][poll] are in! Surprisingly,
impredicative types topped out the list of type system extensions
people want to talk about so I figured I can get the ball rolling.

First things first, all the Haskell code will need the magical
incantation

``` haskell
    {-# LANGUAGE ImpredicativeTypes #-}
```

## What Is Impredicative Polymorphism

We have a lot of extensions that make polymorphism more flexible in
Haskell, `RankNTypes` and `Rank2Types` spring to mind. However, one
important feature lacking is "first class polymorphism".

With impredicative polymorphism `forall`'s become a normal type like
any other. We can embed them in structures, toss them into polymorphic
functions, and generally treat them like any other type.

Readers with a mathematical background will wonder why these are
called "impredicative" types then. The idea is that since we can have
polymorphic types embedded in other structures, we could have
something like

``` haskell
    type T = (Int, forall a. a -> Int)
```

That `a` could assume any time *including `T`*. So each type
definition can quantify over itself which nicely corresponds to the
mathematical notion of impredicativity.

One simple example where this might come up is when dealing with
lenses. Remember lenses have the type

``` haskell
    type Lens {- viciously -} s t a b = forall f. (a -> f b) -> s -> f t
```

If we were to embed lenses in let's say a tuple,

``` haskell
    type TLens a b = (Lens a a (a, b) (a, b), Lens b b (a, b) (a, b))

    foo :: TLens Int Bool
    foo = (_1, _2)
```

We'd need impredicative types because suddenly a polymorphic type has
appeared within a structure.

## Why No One Uses It

Now that we've seen how amazing impredicative polymorphism, let's talk
about how no one uses it. There are two main reasons

 1. GHC's support for impredicative types is fragile at best and
    broken at worst
 2. Avoiding the need for impredicative types is very straightforward

Reason 1 isn't exactly a secret. In fact, SPJ has stated a number of
times that he'd like to deprecate the extension since it's very hard
to maintain with everything else going on.

As it stands right now, our only choice is more or less to type check
a program and add type signatures when GHC decides to instantiate our
beautiful polymorphic type with fresh monomorphic type variables.

For this reason alone, impredicative types aren't really the most
useful thing. The final nail in the coffin is that we can easily make
things more reliable by using newtypes. In lens for example we avoid
impredicativity with

``` haskell
    newtype ScopedLens s t a b =
      ScopedLens {getScopedLens :: Lens s t a b}
```

This means that instead of impredicative types we just need rank N
types, which are much more polished.

## Wrap Up

Well, I'm sorry to be the bearer of bad news for those who filled out
`-XImpredicativeTypes` on the poll, but there you are.

To end on a positive note however, I do know of two example of where
impredicative types did save the day. I've used impredicative type
[exactly once][gist] to handle church lists properly. Lennart
Augustson's [Python DSL][dsl] makes heavy use of them to present a
unified face for variables.

[poll]: http://www.stephendiehl.com/posts/poll.html
[gist]: https://gist.github.com/jozefg/d790c0cd09714cc55a5c
[dsl]: http://augustss.blogspot.com/2011/07/impredicative-polymorphism-use-case-in.html
