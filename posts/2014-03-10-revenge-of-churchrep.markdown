---
title: Church Representations: Part 3
---

To conclude my recent spat of posts on [church](/posts/2014-03-06-church.html)
[representations](/posts/2014-03-07-church-the-sequel.html) I'd like to write one
more.

My last two posts have focused on taking a random value and turning it into
a nifty function that we can use in place of the value. In this post I'll
show to invert the process and given a function, return a value.

This requires a lot more intricate type level programming, so let's start
by turning on our slew of language extensions and importing a few libraries.

``` haskell
    {-# LANGUAGE TypeFamilies,          TypeOperators,     UndecidableInstances #-}
    {-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts     #-}
    {-# LANGUAGE ScopedTypeVariables,   PolyKinds,         DataKinds            #-}
    import Data.Proxy
    import GHC.Generics
```

We'll start by defining a few useful type families across type level lists
(`-XDataKinds` has promoted `[]` for us).

``` haskell
    type family Head (xs :: [k]) :: k
    type instance Head (x ': xs) = x
    type family Tail (xs :: [k]) :: [k]
    type instance Tail (x ': xs) = xs
    
    pHead :: Proxy xs -> Proxy (Head xs)
    pHead = reproxy
    pTail :: Proxy xs -> Proxy (Tail xs)
    pTail = reproxy
    
    type family Append (xs :: [k]) (ys :: [k]) :: [k]
    type instance Append '[] ys = ys
    type instance Append (x ': xs) ys = x ': Append xs ys
    
    type family Reverse (xs :: [k]) :: [k]
    type instance Reverse '[] = '[]
    type instance Reverse (x ': xs) = Append (Reverse xs) (x ': '[])
```

There's not too much of interest here, but it's worth noting the syntax
for type level lists `'[]` and `':`. Additionally, I've made these functions
polykinded for reasons that will become apparent momentarily.

## A High Level Outline

Since the actual procedure for this is much more complex than going to a `Church`,
it's useful to have a high level overview of how we plan on doing this.

The 10,000 foot idea is something like this

 1. Take in the church representation which has the form `(a -> r) -> (b -> c -> r) -> r -> r`
  or similar
 2. Apply the constructors of the type we're interested in to the church representation
 3. Let the representation do the actual work of constructing the value

Our first major hiccup is that we don't actually have access to the constructors.
Instead we'll traverse the generic representation of our type, and produce a type
level list of "breadcrumbs" (I'll explain more shortly) that can be fed to a typeclass
to yield a function which takes either a `U1 p`, `K1 a t p`, or some large combination of `:*:`'s
and returns our value. This serves as our makeshift constructor.

However, this alone isn't enough since we need to provide a function of type
`a -> b -> r` not `M1 foo bar (K1 baz a) :*: M1 foo bar (K1 quux a) -> r`. We need
something akin to the ultimate "uncurry" that'll take in a large product type
component by component and build it up. In order to do this, we'll introduce a "shell"
of a product type where each node is `undefined`, then as we get each new argument,
we'll fill in the corresponding `undefined` in the product type.

This isn't the nicest solution, since we're playing with fire by tossing around `undefined`,
but it's much simpler than the alternatives which is to either somehow convert a tree of product
types into a flat constructor or make each field of the product type maybe and step by step fill
in each field *and* change the type of the product type.

## The Gory Details

Without further delay, let's dive in! First let's create our type level representation
of the "bread-crumbs" we'll use to represent the type's structure

``` haskell
    data Traverse a = Meta a a a | InL a a | InR a a | Term a
```

In our use case, we'll specialize `a` to be `*`, the kind of simple haskell
types.

Now the way to think about this is as a series of directions, each element of the
list a specific instruction on how to navigate the type.

 - `Meta` represents a type wrapped in an `M1` constructor so that
   `M1 a b f p` corresponds to `Meta a b p`
 - `InL` and `InR` represent going left or right in a `:+:` or `:*:`. Going left at `(l :*: r) p` is `InL (r p) p`
 - `Term` is the endpoint of our directions, it holds some leaf in the "tree" we're considering

Now with this in mind, here's how we can construct all possible paths in our types

``` haskell
    type family MakePaths v (m :: [Traverse *]) (r :: [ [Traverse *] ]) :: [[Traverse *] ]
    type instance MakePaths ((:+:) l r p) s all =
      Append (MakePaths (l p) (InL (r p) p ': s) '[])
              (Append (MakePaths (r p) (InR (l p) p ': s) '[]) all)
    type instance MakePaths (M1 a b f p) s all  =
      MakePaths (f p) (Meta a b p ': s) all
    type instance MakePaths (K1 a t p) s all    =  Reverse (Term (K1 a t p) ': s)    ': all
    type instance MakePaths (U1 p) s all        =  Reverse (Term (U1 p) ': s)        ': all
    type instance MakePaths ((:*:) l r p) s all =  Reverse (Term ((:*:) l r p) ': s) ': all
```

This traverse each path, maintaining a stack of all the current stuff seen as `s`
previous paths as `all`. Similarly, we can also reconstruct the original type given a path

``` haskell
    type family ReconstructPath (t :: [Traverse *])
    type instance ReconstructPath (InL r p  ': rest) =
      (WithoutParam (ReconstructPath rest) :+: WithoutParam r) p
    type instance ReconstructPath (InR l p  ': rest) =
      (WithoutParam l :+: WithoutParam (ReconstructPath rest)) p
    type instance ReconstructPath (Meta a b p ': rest) =
      M1 a b (WithoutParam (ReconstructPath rest)) p
    type instance ReconstructPath (Term a     ': '[])  = a
```

Now it should be clear why we need to store the `p` and the other side of `:*:`s and `:+:`s, without
them we couldn't possible reconstruct the type.

We need one final type family before we can start doing real work, this needs to take a path
and return the type that the path leads to, in other words the `Term a` at the end of our list.

Extracting this is pretty mechanical

``` haskell
    type family PathArg (t :: [Traverse *])
    type instance PathArg (Term a     ': '[] ) = a
    type instance PathArg (Meta a b p ': rest) = PathArg rest
    type instance PathArg (InR l p    ': rest) = PathArg rest
    type instance PathArg (InL r p    ': rest) = PathArg rest
```

We only need to explicitly pattern match on `Meta`'s and others because type families
don't have the same convenient "fall through" semantics as normal Haskell functions.

Notice that we expect our list to be terminated by a `Term a ': '[]`, if they
aren't it represents a bug in `MakePaths` and will fail at compile time.

Now we can actually start writing some transformations, first let's define
a function that takes a `[Traverse *]` and the corresponding `PathArg` and
returns our type

``` haskell
    class GPath (p :: [Traverse *]) where
      path :: Proxy p -> PathArg p -> ReconstructPath p
    instance GPath (Term a ': '[]) where
      path _ = id
    instance ((WithoutParam (ReconstructPath rest)) p ~ ReconstructPath rest, GPath rest)
             => GPath (InR r p ': rest) where
      path p a = R1 $ path (pTail p) a
    instance ((WithoutParam (ReconstructPath rest)) p ~ ReconstructPath rest, GPath rest)
             => GPath (InL l p ': rest) where
      path p a = L1 $ path (pTail p) a
    instance ((WithoutParam (ReconstructPath rest)) p ~ ReconstructPath rest, GPath rest)
             => GPath (Meta a b p ': rest) where
      path p a = M1 $ path (pTail p) a
```

This code rolls out pretty similarly to how `ReconstructPath` works, the only difference
is at the end of it all we stick the `PathArg` we've been lugging around with us into the
appropriate "slot" in our type.

Now we just need to take a series of paths and somehow give them to the church representation
with `path` to mimic constructors

``` haskell
    class GBuild (paths :: [[Traverse *] ])f r where
      build :: Proxy paths -> f -> r
    
    -- | Unit case. This represents constructors with no arguments
    instance (ReconstructPath x ~ r, GPath x, GBuild xs f' r, PathArg x ~ U1 p)
             => GBuild (x ': xs) (r -> f') r where
      build p f = build (pTail p) $ f (path (pHead p) U1)
    instance (???) => GBuild (x ': xs) ((f -> g) -> f') r where
      build p f = build (pTail p) $ f (??? $ path (pHead p))
    instance GBuild '[] r r where
      build _ f = f
```

So `build` takes a list of paths by `Proxy`, the church representation `f`,
and returns our result `r`, the last instance is the simplest, it represents
once we've fully applied the church representation and can now just return
it.

The next simplest one is the first one, where we have a constructor with no
arguments so the `PathArg` is `U1 p`. Since we can trivially construct a `U1 p`,
we just do so and give it to `path` ourselves, this creates an `r` that we can
give to the church representation. Recall that for a `Church a r`, no argument
constructors of `a` are simply represented with `r`.

Now the tricky bit, in the second instance we must fill in `???` with something that
takes a product type and converts it to something that will swallow and fill in our
product type step by step.

Our first step in doing this is to create a generic way of creating "empty"
product types

``` haskell
    class GEmpty a where
      empty :: a
    instance GEmpty (U1 p) where
      empty = U1
    instance GEmpty (K1 a t p) where
      empty = K1 (error "Error! The impossible has happened.")
    instance GEmpty (f p) => GEmpty (M1 a b f p) where
      empty = M1 empty
    instance (GEmpty (l p), GEmpty (r p)) => GEmpty ((:*:) l r p) where
      empty = empty :*: empty
```

It's pretty clear how this works. Everything except `K1`'s are filled in
with `empty`'s and `K1` is filled in with something equivalent to `undefined`.

Now we can write a type family to give us the paths to each `K1` in a product type

``` haskell
    type family MakeProdPaths v (m :: [Traverse *]) (r :: [ [Traverse *] ]) :: [[Traverse *] ]
    type instance MakeProdPaths (K1 a t p) s all    = Reverse (Term (K1 a t p) ': s) ': all
    type instance MakeProdPaths (M1 a b f p) s all  = MakeProdPaths (f p) (Meta a b p ': s) all
    type instance MakeProdPaths ((:*:) l r p) s all =
      Append (MakeProdPaths (l p) (InL (r p) p ': s) '[])
              (Append (MakeProdPaths (r p) (InR (l p) p ': s) '[]) all)
```
This should strike the reader as very similar to `MakePaths` because it is. In fact the only real
difference is that we only accept `K1`'s in nodes and that we're branching on `:*:`'s.

Just like `MakePaths`, we'll want a corresponding type class to take a path and a value
and fill in the corresponding bit of our structure.

``` haskell
    class GUpdate (path :: [Traverse *]) a where
      update :: Proxy path -> a -> PathArg path -> a
    instance GUpdate (Term (K1 a t p) ': '[]) (K1 a t p) where
      update _ _ a = a
    instance GUpdate rest (l p) => GUpdate (InL (r p) p ': rest) ((:*:) l r p) where
      update p (l :*: r) a = update (pTail p) l a :*: r
    instance GUpdate rest (r p) => GUpdate (InR (l p) p ': rest) ((:*:) l r p) where
      update p (l :*: r) a = l :*: update (pTail p) r a 
    instance GUpdate rest (f p) => GUpdate (Meta a b p ': rest) (M1 a b f p) where
      update p (M1 f) a = M1 (update (pTail p) f a)
```

Unlike `GPath`, we take in a structure instead of updating it instead of creating
an entirely new one. Notice that we're careful to never be strict in any leaves of the
structure since we intend to use this with `GEmpty` and all those leaves will blow up
if poked.

Now for the really clever bit, we can use `update` to create a function which will
take in an argument and the corresponding path to fill it in in the structure. 

``` haskell
    type family Fill (paths :: [[Traverse *] ]) r
    type instance Fill (x ': xs) r = StripK (PathArg x) -> Fill xs r
    type instance Fill '[] r = r
    
    class GFill (paths :: [[Traverse *] ]) a where
      fill :: Proxy paths -> (a -> r) -> a -> Fill paths r
    instance GFill '[] a where
      fill _ f a = f a
    instance (PathArg x ~ K1 m t p, StripK (PathArg x) ~ t, GUpdate x a, GFill xs a) =>
             GFill (x ': xs) a where
      fill p f a = \x -> fill (pTail p) f $ update (pHead p) a (K1 x)
```
`Fill` represents the type of these functions. While most of this code is as one would
expect, notice that we take a continuation `a -> r` and drag it through `fill` and finally
apply it once we've filled in all the leaves. There is a good reason for this, we intend to
use this with `path`, but we can't compose them since `fill` takes varying amounts of arguments.
Instead, we opt for a bit of continuation passing style and through `path` into `fill` and let
`fill` call it where appropriate.

Now we can fill in that last bit of `GBuild`

``` haskell
    instance ((f -> g) ~ Fill (MakeProdPaths (PathArg x) '[] '[]) r,
              ReconstructPath x ~ r, GEmpty (PathArg x), GBuild xs f' r,
              (GFill (MakeProdPaths (PathArg x) '[] '[]) (PathArg x)),
              GPath x)
             => GBuild (x ': xs) ((f -> g) -> f') r where
      build p f = build (pTail p) $ f (fill (prod p) (path (pHead p)) empty)
        where prod :: forall xs. Proxy xs -> Proxy (MakeProdPaths (PathArg (Head xs)) '[] '[])
              prod _ = Proxy
```

And now we're almost done! We can wrap all of this up into
one function

``` haskell
    fromChurch :: forall a. (Generic a,
                       GBuild (MakePaths (Rep a ()) '[] '[])
                              (Church a (Rep a ()))
                              (Rep a ()))
                  => Church a (Rep a ()) -> a
    fromChurch c = to $ (build p c :: Rep a ())
      where p :: Proxy (MakePaths (Rep a ()) '[] '[])
            p = Proxy
```

And that's it! Automatic reconstruction of types from church representations!
To demonstrate

    > fromChurch (\nothing just -> just True) :: Maybe Bool
       Just True
    > fromChurch (\f -> f 'a' "foo") :: (Char, String)
       ('a', "foo")

Thanks for reading through this (rather dense) series of posts!
Most of the code can be found bundled into a package: [generic-church](http://bitbucket.org/jozefg/generic-church).
