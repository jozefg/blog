---
title: Observations about -XStaticPointers
tags: haskell, types
---

For those who haven't heard, GHC 7.10 is making a brave foray into the
exciting world of distributed computing. To this end, it's made a new
language extension called `-XStaticPointers` to support Cloud Haskell
in a pleasant, first class manner.

If you haven't heard of static pointers before now, it's worth
glancing through the nice [tutorial][tutorial] from ocharles' 24 days
of $(Haskell Related Thing).

The long and short of it is that `-XStaticPointers` gives us this new
keyword `static`. We apply static to an expression and if there are
no closured variables (to be formalized momentarily) then we get
back a `StaticPtr a`. This gives us a piece of data that we can"
serialize and ship over the wire because it has no dependencies.

Now to expand upon this "no closured variables". A thing can only be
fed to `static` if the free variables in the expression are all top
level variables. This forbids us from writing something like

``` haskell
    foo :: StaticPtr a
    foo a = static a
```

Now in all honesty, I'm not super interested in Cloud Haskell. It's
not my area of expertise and I'm already terrified of trying to do
things on one machine. What does interest me a lot though is this
notion of having "I have no free variables" in the type of an
expression. It's an invariant we didn't really have before in
Haskell.

In fact, as I looked more closely it reminded me of something called
box from modal logic.

## A Quick Summary of Modal Logic

I'm not trying to give you a full understanding of modal logic, just a
brief taste.

Modal logic extends our vanilla logic (in Haskell land this is
[constructive logic][why-constructive]) with modalities. Modalities
are little prefixes we tack on the front of a proposition to qualify
its meaning slightly.

For example we might say something like

> If it is possible that it is raining, then I will need an umbrella.

Here we used then modality `possible` to indicate we're not assuming
that it *is* snowing, only that it's conceivable that it is. Because
I'm a witch and will melt in the rain even the possibility of death
raining from the sky will force me to pack my umbrella.

To formalize this a bit, we have our inductive definition of
propositions

    P = ⊥
      | ⊤
      | P ∧ P
      | P ∨ P
      | P ⇒ P
      | □ P

This is the syntax of a particular modal logic with one
modality. Everything looks quite normal up until the last
proposition form, which is the "box" modality applied to some
proposition.

The box modality (the one we really care about for later) means
"necessarily". I almost think of it is a truthier truth if you can buy
that. □ forbids us from using any hypotheses saying something like
`A is true` inside of it. Since it represents a higher standard of
proof we can't use the weaker notion that `A is true`! The rule for
creating a box looks like this to the first approximation

     • ⊢ A
    ———————
    Γ ⊢ □ A

So in order to prove a box something under a set of assumptions Γ, we
have to prove it assuming *none* of those assumptions. In fact, we
find that this is a slightly overly restrictive form for this
judgment, we know that if we have a `□ A` we proved it without
assumptions so if we have to introduce a `□ B` we should be able to
use the assumption that `A is true` for this proof because we know we
can construct one without any assumptions and could just copy paste
that in.

This causes us to create a second context, one of the hypotheses that
`A is valid`, usually notated with a Δ. We then get the rules


       Δ; • ⊢ A          Δ; Γ ⊢ A valid     A valid ∈ Δ
    ———————————————      ——————————————   ———————————————
    Δ; Γ ⊢ □ A true       Δ; Γ ⊢ A true    Δ; Γ ⊢ A valid


    Δ; Γ ⊢ □ A   Δ, A valid; Γ ⊢ B
    ——————————————————————————————
             Δ; Γ ⊢ B


What you should take away from these scary looking symbols is

 1. `A valid` is much stronger than `A true`
 2. Anything inside a □ can depend on valid stuff, but not true stuff
 3. `□ A true` is the same as `A valid`.

This presentation glosses over a fair amount, if your so inclined I'd
suggest looking at Frank Pfenning's [lecture notes][lectures] from his
class entitled "Modal Logic". These actually go at a reasonable pace
and introduce the groundwork for someone who isn't super familiar
with logic.

Now that we've established that there is an interesting theoretical
backing for modal logic, I'm going to drop it on the floor and look at
what Haskell actually gives us.

## That "Who Cares" Bit

Okay, so how does this pertain to `StaticPtr`? Well I noticed that
just like how box drops hypotheses that are "merely true", `static`
drops variables that are introduced by our local context!

This made me think that perhaps `StaticPtr`s are a useful equivalent
to the □ modality! This shouldn't be terribly surprising for PL
people, indeed the course I linked to above expressly mentions □ to
notate "movable code". What's really exciting about this is that there
are a lot more applications of □ then just movable code! We can use it
to notate staged computation for example.

Alas however, it was not to be. Static pointers are missing one
essential component that makes them unsuitable for being □, we can't
eliminate them properly. In modal logic, we have a rule that lets
other boxes depend on the contents of some box. The elimination rule
is much stronger than just "If you give me a `□ A`, I'll give you an
`A`" because it's much harder to construct a `□ A` in the first place!
It's this asymmetry that makes static pointers not quite kosher. With
static pointers there isn't a notion that we can take one static
pointer and use it in another.

For example, we can't write

``` haskell
    applyS :: StaticPtr a -> StaticPtr (a -> b) -> StaticPtr b
    applyS sa sf = static (deRefStaticPtr sf (deRefStaticPtr sa))
```

My initial reaction was that `-XStaticPointers` is missing something,
perhaps a notion of a "static pattern". This would let us say
something like

``` haskell
    applyS :: StaticPtr a -> StaticPtr (a -> b) -> StaticPtr b
    applyS sa sf =
      let static f = sf
          static a = sa
      in static (f a)
```

So this `static` pattern in a keyword would allow us to hoist a
variable into the realm of things we're allowed to leave free in a
static pointer.

This makes sense from my point of view, but less so from that of Cloud
Haskell. The whole point of static pointers is to show a computation
is dependency free after all, static patterns introduce a (limited)
set of dependencies on the thunk that make our lives complicated. It's
not obvious to me how to desugar things so that static patterns can be
compiled how we want them to be, it looks like it would require some
runtime code generation which is a no-no for Haskell.

My next thought was that maybe `Closure` was the answer, but that
doesn't actually work either! We can introduce a closure from an
arbitrary serializable term which is exactly what we *don't* want from
a model of □! Remember, we want to model closed terms so allowing us
to accept an arbitrary term defeats the point.

It's painfully clear that `StaticPtr`s are very nearly □s, but not
quite! Whatever `Box` ends up being, we'd want the following interface

``` haskell
    data Box a

    intoBox :: StaticPtr a -> Box a
    closeBox :: Box (a -> b) -> Box a -> Box b
    outBox :: Box a -> a
```

The key difference from `StaticPtr`'s being `closeBox`. Basically this
gives us a way to say "I have something that's closed except for one
dependency" and we can fill that dependency with some other closed
term.

This turns something like

``` haskell
    let static x = sx in static y
```

into

``` haskell
    intoBox (static (\x -> y)) `closeBox` intoBox sx
```

If you read the tutorial, you'll notice that this is most of the
implementation of `Closure`! Following our noses we define

``` haskell
    data Box a where
      Pure  :: StaticPtr a -> Box a
      Close :: Box (a -> b) -> Box a -> Box b
```

This is literally the dumbest implementation of `Box` I think is
possible, but it actually works just fine.

``` haskell
    intoBox = Pure
    closeBox = Close

    outBox :: Box a -> a
    outBox (Pure a) = deRefStaticPtr a
    outBox (Close f a) = outBox f (outBox a)
```

which would seem to be modal logic in Haskell.

## Wrap Up

To be honest, I'm not sure yet how this is useful. I'm kinda swamped
with coursework at the moment (new semester at CMU) but it seems like
a new and fun thing to play with.

I've stuck the code at [jozefg/modal](http://github.com/jozefg/modal)
if you want to play with it. Fair warning that it only compiles with
GHC >= 7.10 because we need static pointers.

Cheers.

[tutorial]: https://ocharles.org.uk/blog/guest-posts/2014-12-23-static-pointers.html
[lectures]: http://www.cs.cmu.edu/~fp/courses/15816-s10/
[why-constructive]: /posts/2015-01-09-constructivism.html
