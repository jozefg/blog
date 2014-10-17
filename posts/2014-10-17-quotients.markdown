---
title: Notes on Quotients Types
tags: types
---

Lately I've been reading a lot of type theory literature. In effort
to help my future self, I'm going to jot down a few thoughts on
quotient types, the subject of some recent google-fu.

## But Why!

The problem quotient types are aimed at solving is actually a very
common one. I'm sure at some point or another you've used a piece of
data you've wanted to compare for equality. Additionally, that data
properly needed some work to determine whether it was equal to
another piece.

A simple example might would be representing rational numbers. A
rational number is a fraction of two integers, so let's just say

``` haskell
    type Rational = (Integer, Integer)
```

Now all is well, we can define a `Num` instance and what not. But what
about equality? Clearly we want equivalent fractions to be equal. That
should mean that `(2, 4) = (1, 2)` since they both represent the same
number.

Now our implementation has a sticky point, clearly this isn't the case
on its own! What we really want to say is "`(2, 4) = (1, 2)` up to
trivial rejiggering".

Haskell's own `Rational` type solves this by not exposing a raw
tuple. It still exists under the hood, but we only expose smart
constructors that will reduce our fractions as far as possible.

This is displeasing from a dependently typed setting however, we want
to be able to formally prove the equality of some things. This
"equality modulo normalization" leaves us with a choice. Either we can
really provide a function which is essentially

``` agda
    foo : (a b : Rational)
        -> Either (reduce a = reduce b) (reduce a /= reduce b)
```

This doesn't really help us though, there's no way to express that `a`
should be observationally equivalent to `b`. This is a problem
seemingly as old as dependent types: How can we have a simple
representation of equality that captures all the structure we want and
none that we don't.

Hiding away the representation of rationals certainly buys us
something, we can use a smart constructor to ensure things are
normalized. From there we could potentially prove a (difficult)
theorem which essentially states that

``` agda
    =-with-norm : (a b c d : Integer)
                -> a * d = b * c -> mkRat a b = mkRat c d
```

This still leaves us with some woes however, now a lot of computations
become difficult to talk about since we've lost the helpful notion
that `denominator o mkRat a = id` and similar. The lack of
transparency shifts a lot of the burden of proof onto the code privy
to the internal representation of the type, the only place where we
know enough to prove such things.

Really what we want to say is "Hey, just forget about a bit of the
structure of this type and just consider things to be identical up to
`R`". Where `R` is some equivalence relation, eg

  1. `a R a`
  2. `a R b` implies `b R a`
  3. `a R b` and `b R c` implies `a R c`

If you're a mathematician, this should sound similar. It's a lot like
how we can take a set and partition it into equivalence classes. This
operation is sometimes called "quotienting a set".

For our example above, we really mean that our rational is a type
quotiented by the relation `(a, b) R (c, d)` iff `a * c = b * d`.

Some other things that could potentially use quotienting

 - Sets
 - Maps
 - Integers
 - Lots of Abstract Types

Basically anything where we want to hide some of the implementation
details that are irrelevant for their behavior.

## More than Handwaving

Now that I've spent some time essentially waving my hand about
quotient types what are they? Clearly we need a rule that goes
something like

     Γ ⊢ A type, E is an equivalence relation on A
    ———————————————–———————————————————————————————
            Γ ⊢ A // E type

Along with the typing rule

        Γ ⊢ a : A
    ——————————————————
      Γ ⊢ a : A // E

So all members of the original type belong to the quotiented type, and
finally

      Γ ⊢ a : A, Γ ⊢ b : A, Γ ⊢ a E b
    –——————————————–——————————————————
             Γ ⊢ a ≡ b : A // E

Notice something important here, that `≡` is the fancy shmancy
judgmental equality baked right into the language. This calls into
question decidability. It seems that `a E b` could involve some
non-trivial proof terms.

More than that, in a constructive, proof relevant setting things can be
a bit trickier than they seem. We can't just define a quotient to be
the same type with a different equivalence relation, since that would
imply some icky things.

To illustrate this problem, imagine we have a predicate `P` on a type
`A` where `a E b` implies `P a ⇔ P b`. If we just redefine the
equivalence relation on quotes, `P` would not be a wellformed
predicate on `A // E`, since `a ≡ b : A // E` doesn't mean that
`P a ≡ P b`. This would be unfortunate.

Clearly some subtler treatment of this is needed. To that end I found
[this paper][that-paper] discussing some of the handling of NuRPL's
quotients enlightening.

## How NuPRL Does It

The paper I linked to is a discussion on how to think about quotients
in terms of other type theory constructs. In order to do this we need
a few things first.

The first thing to realize is that NuPRL's type theory is different
than what you are probably used to. We don't have this single magical
global equality. Instead, we define equality inductively across the
type. This notion means that our equality judgment doesn't have to be
natural in the type it works across. It can do specific things at
each case. Perhaps the most frequent is that we can have functional
extensionality.

    f = g ⇔ ∀ a. f a = g a

Okay, so now that we've tossed aside the notion of a single global
equality, what else is new? Well something new is the lens through
which many people look at NuRPL's type theory: PER semantics. Remember
that PER is a relationship satisfying

  1. `a R b → then b R a`
  2. `a R b ∧ b R c → a R c`

In other words, a PER is an equivalence relationship that isn't
necessarily reflexive at all points.

The idea is to view types not as some opaque "thingy" but instead to
be partial equivalence relations across the set of untyped lambda
calculus terms. Inductively defined equality falls right out of this
idea since we can just define `a ≡ b : A` to be equivalent to
`(a, b) ∈ A`.

Now another problem rears it head, what does `a : A` mean? Well even
though we're dealing with PERs, but it's quite reasonable to say
something is a member of a type if it's reflexive. That is to say each
relation is a full equivalence relation for the things we call members
of that type. So we can therefore define `a : A` to be `(a, a) ∈ A`.

Another important constraint, in order for a type family to be well
formed, it needs to respect the equality of the type it maps
across. In other words, for all `B : A → Type`, we have `(a, a') ∈ A'
⇒ (B a = B a') ∈ U`. This should seem on par with how we defined
function equality and we call this "type functionality".

Let's all touch on another concept: squashed types. The idea is to
take a type and throw away all information other than whether or not
it's occupied. There are two basic types of squashing, extensional or
intensional. In the intensional we consider two squashed things equal
if and only if the types they're squashing are equal

         A = B
      ————————————
       [A] = [B]

Now we can also consider only the behavior of the squashed type, the
extensional view. Since the only behavior of a squashed type is simply
existing, our extensional squash type has the equivalence

       ∥A∥ ⇔ ∥B∥
       ————————–
        ∥A∥ = ∥B∥

Now aside from this, the introduction of these types are basically the
same: if we can prove that a type is occupied, we can grab a squashed
type. Similarly, when we eliminate a type all we get is the trivial
occupant of the squashed type, called •.

        Γ ⊢ A
       ———————
       Γ ⊢ [A]

        Γ, x : |A|, Δ[̱•] ⊢ C[̱•]
      ——————————————————————————
        Γ, x : |A|, Δ[x] ⊢ C[x]


What's interesting is that when proving an equality judgment, we can
unsquash obth of these types. This is only because NuRPL's equality
proofs computationally trivial.

Now with all of that out of the way, I'd like to present two typing
rules. First

      Γ ⊢ A ≡ A';  Γ, x : A, y : A ⊢ E[x; y] = E'[x; y]; E and E' are PERS
      ————————————————————————————————————————————————————————————————————
                          Γ ⊢ A ‌// E ≡ A' // E'

In English, two quotients are equal when the types and their
quotienting relations are equal.

     Γ, u : x ≡ y ∈ (A // E), v :  ∥x E y∥, Δ[u] ⊢ C [u]
     ———————————————————————————————————————————————————–
           Γ, u : x ≡ y ∈ (A // E), Δ[u] ⊢ C [u]

There are a few new things here. The first is that we have a new
`Δ [u]` thing. This is a result of dependent types, can have things in
our context that depend on `u` and so to indicate that we "split" the
context, with `Γ, u, Δ` and apply the depend part of the context `Δ`
to the variable it depends on `u`.

Now the long and short of this is that when we're of this is that when
we're trying to use an equivalence between two terms in a quotient, we
only get the squashed term. This done mean that we only need to
provide a squash to get equality in the first place though

    Γ ⊢ ∥ x E y  ∥; Γ ⊢ x : A; Γ ⊢ y : A
    ——————————————————————————————————–
          Γ ⊢ x ≡ y : A // E

Remember that we can trivially form an `∥ A ∥` from `A`'.

Now there's just one thing left to talk about, using our quotiented
types. To do this the paper outlines one primitive elimination rule
and defines several others.

    Γ, x : A, y : A, e : x E y, a : ND, Δ[ndₐ{x;y}] ⊢ |C[ndₐ{x;y}]|
    ——————————————————————————————————————————————————————————————–
                   Γ, x : A // E, Δ[x] ⊢ |C[x]|

`ND` is a admittedly odd type that's supposed to represent
nondeterministic choice. It has two terms, `tt` and `ff` and
they're considered "equal" under `ND`. However, `nd` returns its
first argument if it's fed `tt` and the second if it is fed
`ff`. Hence, nondeterminism.

Now in our rule we use this to indicate that if we're eliminating
some quotiented type we can get *any* value that's considered
equal under `E`. We can only be assured that when we eliminate a
quotiented type, it will be related by the equivalence relation to
`x`. This rule captures this notion by allowing us to randomly choose
some `y : A` so that `x E y`.

Overall, this rule simply states that if `C` is occupied for any term
related to `x`, then it is occupied for `C[x]`.

## Wrap up

As with my last post, here's some questions for the curious reader to
pursue

 - What elimination rules can we derive from the above?
 - If we're of proving equality can we get more expressive rules?
 - What would an extensional quotient type look like?
 - Why would we want intensional or extensional?
 - How can we express quotient types with higher inductive types from
   HoTT

The last one in particularly interesting.

*Thanks to Jon Sterling for proof reading*

[that-paper]: http://www.nuprl.org/documents/Nogin/QuotientTypes_02.pdf
