---
title: F'd Up Recursion
tags: Haskell
---

One of the common pieces of folklore in the functional programming
community is how one can cleanly formulate recursive types with
category theory. Indeed, using a few simple notions we can build a
coherent enough explanation to derive some concrete benefits.

In this post I'll outline how one thinks of recursive types and then
we'll discuss some of the practical ramifications of such thoughts.

## Precursor

Before we dive in, it's important to flesh out a vocabulary for
talking about category theory as we care about it. The first thing to
notice is that we define a category as a set of objects (I'll use `A`,
`B`, and `C` to indicate them) and a set of arrows between such
objects (I'll use `f`, `g`, and `h`). For each arrow we define
`dom(f)` which gives the domain in `F` and `cod(f)` which gives the
codomain.

Further, there must be an arrow for each object, `idₐ`, so that
`cod(idₐ) = dom(idₐ) = a` for each object `a`. There is also an
operation `∘ : (B -> C) -> (A -> B) -> (A -> C)` which is associative
and has `idₐ` as its unit.

None of this should be earth shattering, but a little precision never
hurt. Now we can define a functor `F : C → D` to be a map between two
categories `C` and `D` so that `F` can map objects in `C` to objects
in `D` and arrows in `C` to arrows in `D`. Further, for all
`f : A → B` in `C`, `F f : F A → F B` and `F (f ∘ g) = F f ∘ F g`.

Now from here we can talk about endofunctors, which are functors whose
domain and codomain are the same.
*spoiler: These are the ones we care about in Haskell*. An interesting
notion that comes from endofunctors is that of algebras. An algebra in
this sense is a pair of an object `C`, and a map `F C → C`. Here `F`
is called the "signature" and `C` is called the carrier.

If you curious about why these funny terms, in abstract algebra we
deal with algebras which are comprised of a set of distinguished
elements, functions, and axioms called the signature. From there we
look at sets (called carriers) which satisfy the specification. We can
actually cleverly rearrange the specification for something like a
group into an endofunctor! It's out of scope for this post, but
interesting if algebras your thing.

Now we can in fact define a category for F-algebras. in such a
category an object is `α : F A → A` and each arrow is a triplet.

 - normal arrow `f : A → B`
 - An F-algebra `α : F A → A`
 - Another F-algebra `β : F B → B`

So that `f ∘ α = β ∘ F f`. In picture form

             F f
    F A ———————————————–→ F B
     |                    |
     |                    |
     | α                  | β
     ↓                    ↓
     A —————————————————→ B
               f

commutes. I generally elide the fact that we're dealing with triplets
and instead focus on the arrow, since that's the interesting bit.

Now that we've established F-algebras, we glance at one more
thing. There's one more concept we need, the notion of initial
objects. An initial object is an... object, `I` in a category so that
for any object `C`


              f
     I - - - - - - - - → C

So that `f` is unique.

Now what we're interested in investigating is the initial object in
the category of F-algebras. That'd mean that

               α
    F I ————————————————–→ I
     |                     |
     |
     | F λ                 | λ
     |
     ↓                     ↓
    F C —————————————————→ C

Commutes only for a unique λ.

## A List is just an Initial Object in the Category of F-Algebras.

*What's the problem?*

Now, remembering that we're actually trying to understand recursive
types, let's circle back to thinking of those. We can think of
recursive types as solutions to certain equations. In fact, our types
are what are called the *least fixed point* solutions. Let's say we're
looking at `IntList`. We can imagine it defined as

``` haskell
    data IntList = Cons Int IntList | Nil
```

We can in fact, factor out the recursive call in `Cons` and get

``` haskell
    data IntList a = Cons Int a | Nil
                   deriving Functor
```

Now we can represent a list of length 3 as something like

    type ThreeList = IntList (IntList (IntList Void))

Which is all well and good, but we really want arbitrary length
list. We want a solution to the equation that

    X = IntList X

We can view such a type as a set `{EmptyList, OneList, TwoList,
ThreeList ... }`. Now how can we actually go about saying this? Well
we need to take a fixed point of the equation! This is easy enough in
Haskell since Haskell's type system is unsound.

    data FixedPoint f = Fix {unfix :: f (FixedPoint f)}

Now we can regain our normal representation of lists with

    type List = FixedPoint IntList

To see how this works

``` haskell
    out :: FixedPoint IntList -> [Int]
    out (Fix f) = case fmap out f of
                    Nil -> []
                    Cons a b -> a : b

    in :: [Int] -> FixedPoint IntList
    in [] = Nil
    in (x : xs) = Fix (Cons x (in xs))
```

Now this transformation is interesting for one reason in particular,
`IntList` is a functor. Because of this, we can formulate an F-algebra
for `IntList`.

``` haskell
    type ListAlg a = IntList a -> a
```

Now we consider what the initial object in this category would be.
It'd be something `I` so that we have a function

``` haskell
    cata :: Listalg a -> (I -> a)
    cata :: (List a -> a) -> I -> a
    cata :: (Either () (a, Int) -> a) -> I -> a
    cata :: (() -> a) -> ((a, Int) -> a) -> I -> a
    cata :: a -> (Int -> a -> a) -> I -> a
    cata :: (Int -> a -> a) -> a -> I -> a
```

Now that looks sort of familiar, what's the type of `foldr` again?

``` haskell
    foldr :: (a -> b -> b) -> b -> [a] -> a
    foldr :: (Int -> a -> a) -> a -> [Int] -> a
```

So the arrow we get from the initiality of `I` is precisely the same
as `foldr`! This leads us to believe that maybe the initial object for
F-algebras in Haskell is just the least fixed point, just as `[Int]`
is the least fixed point for `IntList`.

To confirm this, let's generalize a few of our definitions from before

``` haskell
    type Alg f a = f a -> a
    data Fix f = Fix {unfix :: f (Fix f)}

    type Init f = Alg f (Fix f)

    cata :: Functor f => Alg f a -> Fix f -> a
    cata f = f . fmap (cata f) . unfix
```

Our suspicion is confirmed, the fixed point of an functor is indeed
the initial object. Further more, we can easily show that initial
objects are unique up to isomorphism (exercise!) so anything that can
implement `cata` is isomorphic to the original, recursive definition
we were interested in.


## When The Dust Settles

Now that we've gone and determined a potentially interesting fact
about recursive types, how can we use this knowledge? Well let's start
with a few things, first is that we can define a truly generic fold
function now:

    fold :: Functor f => (f a -> a) -> Fix f -> a

This delegates all the messy details of how one actually thinks about
handling the "shape" of the container we're folding across by
relegating it to the collapsing function `f a -> a`.

While this may seem like a small accomplishment, it does mean that we
can build off it to create data type generic programs that can be
fitted into our existing world.

For example, what about mutual recursion. Fold captures the notion of
recurring across one list in a rather slick way, however, recurring
over two in lockstep involves a call to zip and other fun and
games. How can we capture this with `cata`?

We'd imagine that the folding functions for such a scenario would have
the type

    f (a, b) -> a
    f (a, b) -> b

From here we can build

    muto :: (f (a, b) -> a) -> (f (a, b) -> b) -> Fix f -> (a, b)
    muto f g = cata ((,) <$> f <*> g)

Similarly we can build up [oodles][rec-scheme] of combinators for
dealing with folding all built on top of `cata`!

That unfortunately sounds like a lot of work! We can shamelessly
free-load of the hard work of others thanks to hackage though. In
particular, the package `recursion-schemes` has built up a nice little
library for dealing with initial algebras. There's only one big twist
between what we've laid out and what it does.

One of the bigger stumbling blocks for our library was factoring out
the nice recursive definition for types that we write into the
somewhat unnatural contortion of the functorfied version. Really it's
not realistic to write all your types this way. To help simplify the
process `recursion-schemes` provides a type family called `Base` which
takes a type and returns its functorfied version. We can imagine
something like

``` haskell
    data instance Base [a] b = Cons a b | Nil
```

This simplifies the process of actually using all these combinators
we're building. To use recursion-schemes, all you need to is define
such an instance and write `project :: t -> Base t t`. After that it's
all kittens and recursion.

## Wrap Up

So dear reader, where are we left? We've got a new interesting
formulation of recursive types that yields some interesting results
and power. There's one interesting chunk we've neglected though: what
does unfolding look like?

It turns out there's a good story for this as well, unfolding is the
operation (anamorphism) defined by a terminal object in a category. A
terminal object is the precise dual of an initial one. You can notice
this all in recursion-schemes which features `ana` as well as `cata`.


[rec-scheme]: https://hackage.haskell.org/package/recursion-schemes-4.1/docs/Data-Functor-Foldable.html#g:3
