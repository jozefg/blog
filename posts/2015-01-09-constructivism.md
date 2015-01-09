---
title: Why Constructive Logic
tags: types
---

Continuing on my quest of writing about my poorly thought out
comments, let's talk about constructive logic. A lot of people in and
around the Haskell/FP community will make statements like

> The Curry-Howard isomorphism means that you're proving things in
> constructive logic.

Usually absent from these remarks is a nice explanation of *why*
constructive logic matches up with the programming we know and love.

In this post I'd like to highlight what constructive logic is intended
to capture and why this corresponds so nicely with programming.

## A Bit of History

First things first, let's discuss the actual origin of constructive
logic. It starts with a mathematician and philosopher named
Brouwer. He was concerned trying to give an answer to the question
"What does it mean to know something to be true" where something is
defined as a mathematical proposition.

He settled on the idea of proof being a sort of subjective and
personal thing. I know something is true if and only if I can
formulate some intuitive proof of it. When viewed this way, the proof
I scribble down on paper doesn't actually validate something's
truthfulness. It's merely a serialization of my thought process for
validating its truthfulness.

Notice that this line of reasoning doesn't actually specify a precise
definition of what verifying something intuitively means. I interpret
this idea as something slightly more meta then any single formal
system. Rather, when looking a formal system, you ought to verify that
its axioms are admissible by your own intuition and then you may go on
to accept proofs built off of these axioms.

Now after Brouwer started talking about these ideas Arend Heyting
decided to try to write down a logic that captured this notion of
"proof is intuition". The result was this thing called constructive
logic.

## Constructive Logic

The core idea of constructive logic is replacing the notion of truth
found in classical logic with an intuitionist version. In a classical
logic each proposition is either true or false, regardless of what we
know about it.

In our new constructive system, a formula cannot be assigned either
until we have direct evidence of it. It's not that there's a magical
new boolean value, {true, false, i-don't-know}, it's just not a
meaningful question to ask. It doesn't make sense in these logics to
say "`A` is true" without having a proof of `A`. There isn't
necessarily this Platonic notion of truthfulness, just things we as
logicians can prove. This is sometimes why constructive logic is
called "logic for humans".

The consequences of dealing with things in this way can be boils down
to a few things. For example, we now know that

 1. If `∃x. A(x)` can be proven, then there is some term *which we can
    readily produce* `t` so that `A(t)` is provable
 2. If `A ∨ B` can be proven then either `A` or `B` is provable and we
    know which. (note that ∨ is the symbol for OR)

These make sense when you realize that `∃x. A(x)` can only be proven if
we have a direct example of it. We can't indirectly reason that it
really ought to exist or merely claim that it must be true in one of a
set of cases. We actually need to introduce it by proving an example of
it. When our logic enforces this of course we can produce that
example!

The same goes for `A ∨ B`, in our logic the only way to prove
`A ∨ B` is to either provide a proof of `A` or provide a proof of
`B`. If this is the only way to build a `∨` we can always just point
to how it was introduced!

If we extend this to and, `∧`: The only way to prove `A ∧ B` is to
prove both `A` and `B`. If this is the only way to get to a proof of
`A ∧ B` then of course we can get a proof of `A` from `A ∧ B`. `∧` is
just behaving like a pair of proofs.

All of this points at one thing: our logic is structured so that we
can only prove something when we directly prove it, that's the spirit
of Brouwer's intuitionism that we're trying to capture.

There are a lot of different incarnations of constructive logic, in
fact pretty much every logic has a constructive cousin. They all share
this notion of "We need a direct proof to be true" however. One thing
to note that is that some constructive logics conflict a bit with
intuitionism. In particular some of these logics add axioms that
conflict with the idea that "we only know what we have a proof
for". While intuitionism might have provided some of the basis for
constructive logics gradually people have poked and pushed the
boundaries away from *just* Brouwer's intuitionism.  For example both
Markov's principle and Church's thesis state something about *all*
computable functions. While they may be reasonable statements we can't
give a satisfactory proof for them. This is a little confusing I know
and I'm only going to talk about constructive logics that Brouwer
would approve of.

I encourage the curious reader to poke further at this, it's rather
cool math.

## Who on Earth Cares?

Now while constructive logic probably sounds reasonable, if weird, it
doesn't immediately strike me as particularly useful! Indeed, the main
reason why computer science cares about constructivism is because we
all use it already.

To better understand this, let's talk about the Curry-Howard
isomorphism. It's that thing that wasn't really invented by either
Curry or Howard and some claim isn't best seen as an isomorphism,
naming is hard. The Curry-Howard isomorphism states that there's a
mapping from a type to a logical proposition and from a program to a
proof.

To show some of the mappings for types

``` haskell
    CH(Either a b) = CH(a) ∨ CH(b)
    CH((a, b))     = CH(a) ∧ CH(b)
    CH( () )       = ⊤ -- True
    CH(Void)       = ⊥ -- False
    CH(a -> b)     = CH(a) → CH(b)
```

So a program with the type `(a, b)` is really a proof that `a ∧ b` is
true. Here the truthfulness of a proposition really means that the
corresponding type can be occupied by a program.

Now, onto why this logic we get is constructive. Recall our two
conditions for a logic being constructive, first is that if `∃x. A(x)`
is provable then there's a specific `t` where `A(t)` is provable.

Under the Curry Howard isomorphism, ∃ is mapped to existential types
(I wonder how that got its name :). That means that a proof of
`∃x. A(x)` is something like

``` haskell
    -- Haskell ex. syntax is a bit gnaryl :/
    data Exists f = forall x. Exists f x

    ourProof :: Exists F
    ourProof = ...
```

Now we know the only way to construct an `Exists F` is to use the
constructor `Exists`. This constructor means that there is at least
one specific type for which we could prove `f x`. We can also easily
produce this term as well!

``` haskell
    isProof :: Exists f -> (f x -> c) -> c
    isProof (Exists x) cont = cont x
```

We can always access the specific "witness" we used to construct this
`Exists` type with pattern matching.

The next law is similar. If we have a proof of `a ∧ b` we're supposed
to immediately be able to produce a proof of `a` or a proof of `b`.

In programming terms, if we have a program `Either a b` we're supposed
to be able to immediately tell whether this returns `Right` or `Left`!
We can make some argument that one of these must be possible to
construct but we're not sure which since we have to be able to
actually run this program! If we evaluate a program with the type
`Either a b` we're guaranteed to get either `Left a` or `Right b`.

## The "Negative" Definition of Constructive Logic

There are a few explanations of constructive logic that basically
describe it as "Classical logic - the law of excluded middle". More
verbosely, a constructive logic is just one that forbids

 1. `∀ A. A ∨ ¬ A` being provable (the law of excluded middle, LEM)
 2. `∀ A. ¬ (¬ A) → A` being provable (the law of double negation)

I carefully chose the words "being provable" because we can easily
introduce these as a hypothesis to a proof and still have a sound
system. Indeed this is not uncommon when working in Coq or
Agda. They're just not a readily available tool. Looking at them, this
should be apparent as they both let us prove something without
directly proving it.

This isn't really a defining aspect of constructivism, just a natural
consequence. If we need a proof of `A` to show `A` to be true if we
admit `A ∨ ¬ A` by default it defeats the point. We can introduce `A`
merely by showing `¬ (¬ A)` which isn't a proof of `A`! Just a proof
that it really ought to be true.

In programming terms this is saying we can't write these two
functions.

``` haskell
    data Void

    doubleNeg :: ((a -> Void) -> Void) -> a
    doubleNeg = ...

    lem :: Either a (a -> Void)
    lem = ...
```

For the first one we have to choices, either we use this
`(a -> Void) -> Void` term we're given or we construct an `a` without
it. Constructing an arbitrary `a` without the function is just
equivalent to `forall a. a` which we know to be unoccupied. That means
we have to use `(a -> Void) -> Void` which means we have to build an
`a -> Void`. We have no way of doing something interesting with that
supplied `a` however so we're completely stuck! The story is similar
with `lem`.

## Wrap Up

Hopefully this helps clarify what exactly people mean when they say
Haskell corresponds to a constructive logic or programs are
proofs. Indeed this constructivism gives rise to a really cool thing
called "proof relevant mathematics". This is mathematics done purely
with constructive proofs. One of the latest ideas to trickle from
mathematics to computers is homotopy type theory where we take a proof
relevant look at identity types.

Before I wrap up I wanted to share one funny little thought I
heard. Constructive mathematics has found a home in automated proof
systems. Imagine Brouwer's horror at hearing we do "intuitionist"
proofs that no one will ever look at or try to understand beyond some
random mechanical proof assistant!

*Thanks to Jon Sterling and Darryl McAdam's for the advice and insight*
