---
title: Faking Existentials with Rank N Types
tags: haskell, types
---

GHC has a language extension call `ExistentialQuantification`. This lets
users write "existential" types. There are lots of good explanations
of what an existential type is, but to briefly summarize: an existential
type allows the callee to choose the type versus the caller.

As an example, if we have

``` haskell
    {-# LANGUAGE ExistentialQuantification #-}
    data NotExist a = NotExist a
    data Exists = forall a. Show a => Exists a

    normal :: Show a => NotExist a
    normal = NotExist undefined

    exists :: Exists
    exists = Exists "The callee chose this"
```

With `normal` the caller gets to choose what `a` is and so we have to
use `undefined`, otherwise we'd have to construct an arbitrary `Show a => a`.

With `exists`, the callee get's to choose what is boxed up in `Exists`, the
caller can't control anything about it.

Now, in logic existentials correspond to propositions like
`∃ x∈ℕ. x > 0 ∧ x < 2` or in English,
"There exists an `x` such that `x` > 0 and `x` < 2". Normal haskell types
like `NotExists` correspond more to propositions like `∀ x∈ℕ. x < x + 1`.

Interestingly, we can actually define `∃` in terms of `∀`.

    ∃ x∈A. P(x) = ∀ Q. (∀ c∈A. P(c) → Q) → Q

In English, the proposition that "There exists an `x` in `A` so that `P(x)`" is
equivalent to "For all propositions `Q`, if for all `c`, `P(c)` implies `Q`, then `Q`."

This can be translated to Haskell!

We'll need to enable rank n types since our definition for `∃` uses nested `∀`s.
Additionally, we'll use constraint kinds and impredicative polymorphism since we'll want to pass around typeclass constraints
and store polymorphic values in lists.

``` haskell
    {-# LANGUAGE RankNTypes, ConstraintKinds #-}
    {-# LANGUAGE KindSignatures, ImpredicativeTypes #-}
    import GHC.Prim (Constraint)

    type Exists c = forall x. (forall a. c a => a -> x) -> x
```

Here `P(x)` becomes the proposition `c a => a`. Proving this "proposition" is done by providing a value of type `a`,
this is sometimes called "witnessing".

If this jump has left you baffled, try doing a bit of research on the "Curry Howard Isomorphism"
and remembering that the type `c a => a` is really the same as the pair `(Dict, a)` where `Dict` is
the record of all the function in the typeclass `c`.

With this intuition (terrible pun for constructionists) we can write a function to construct a `Exists c` given
an `c a => a`.

``` haskell
    exists :: c a => a -> Exists c
    exists witness cont = cont witness
```

Now we can actually write some code using this

``` haskell
    -- This needs impredicative polymorphism
    showables :: [Exists Show]
    showables = [ exists "string"
                , exists 'c'
                , exists ()
                , exists True]
```

So now we've got a list of `Exists Show`s so let's figure out how to use it.
Let's write a function that takes a function `forall a. c a => a -> b` and
returns a function `Exists c -> b`.

``` haskell
    withExists :: (forall a. c a => a -> b) -> Exists c -> b
    withExists cont existential = existential cont
```

Now we can write

``` haskell
    main = mapM_ (withExists print) showables
```

Which outputs

    "string"
    'c'
    ()
    True

Just as expected!

And there you have it, existential types cobbled together from a few other extensions.
