----------
title: Representable Functors
tags: haskell, math
----------

Representable functors are a powerful tool in category theory. As it turns out,
they're pretty useful in Haskell as well. Here's a few examples of what they are
and how to use them

First, some definition. We're interested in `Hask` which the category where objects are types
and arrows are functions. A representable functor for us, is a special functor from
`Hask -> Hask` (an endofunctor). Now when we apply category theory to Haskell, we also pretend
`Hask` is `Set` (The category of sets). There's a type of functor called a `hom-functor`.
It's a functor that looks like this

``` haskell
    newtype Hom a = (->) a
    -- Hom a b = a -> b
```

Now, a `Hom` implements `Functor` like this

``` haskell
    instance Functor Hom where
        fmap f hom = f . hom
```

In other words, `Hom a` takes an object `b` to the set of all morphisms `a -> b`. It
takes an arrow `b -> c` to the function `Hom a b -> Hom a c` using composition. Nothing
stunning yet.

Now consider some arbitrary functor `F`. Suppose there exists an object `a` so that
`F` is isomorphic to `Hom a`. What would this look like?

``` haskell
    type family Obj (f :: * -> *) :: *
    class Functor f => HomIso f where
      toHom :: f a -> Hom (Obj f) a
      toF   :: Hom (Obj f) a -> f a
```

And we have the laws that

``` haskell
    toHom . toF = id
    toF . toHom = id
```

Then `f` is a representable functor. From now on, I will refer to `HomIso` as `Repr` to emphasize
this. The simplest representable functor is of course `Hom a`.

Let's notice some useful properties of representable functors.

``` haskell
    lookup :: Repr f => f a -> Obj f -> a
    lookup = toHom
```

Our functor can look things up! Cool! Let's use this idea to guide us
to finding some simple representable functors. Let's look at a trivial case

``` haskell
    newtype Identity a = Identity {runIdentity :: a}
                       deriving(Eq, Show, Functor)

    newtype Unit = Unit
    type instance Obj Identity = Unit

    instance Repr Identity where
      toHom (Identity a) = const a
      toF f = Identity $ f Unit
```

Since `Identity` has only one value, `Unit` indexes it exactly. A more complicated example

``` haskell
    data Prod a = Prod a a
                deriving(Eq, Show, Functor)

    data Two = InL | InR
    type instance Obj Prod = Two

    instance Repr Prod where
      toHom (Prod a _) InL  = a
      toHom (Prod _ a) InR = b
      toF hom = Prod (hom InL) (hom InR)
```

This is all quite well, but what about infinite data structures? This is Haskell!
we want those too.

``` haskell
    data Forever a = Cons a (Forever a)
                   deriving (Functor)

    data Nat = Z | S Nat
    type instance Obj Forever = Nat

    instance Repr Forever where
      toHom (Cons a as) Z = a
      toHom (Cons a as) (S n) = toHom as n

      toF f = cs z
        where cs n = Cons (f n) (cs (S n))
```

Since `Forever` goes, well, forever. It can be keyed with natural numbers,
which we represent here with `Nat`. Then `toHom` is classic recursion and
`toF` is classic co-recursion.

There are tons more of these, but hopefully now you're getting the idea. Here's
another cool thought

``` haskell
    switch :: (Repr f, Functor g) => g (f a) -> f (g a)
    switch g = toF $ \obj -> fmap ($ obj) hom
      where hom = fmap toHom g
```

Wait a moment, what if `f` and `g` where both `Repr` instances? Then

``` haskell
    switch . switch = id
```

Neat! We can use representable functors to switch around functors.

Now, what about applicatives, can we use a representative functor to build one?

``` haskell
    -- To keep type classes from getting confused
    newtype Wrap f a = Wrap {unWrap :: f a}

    instance (Repr f) => Applicative (Wrap f) where
      pure    = toF . const
      f <*> a = toF $ \obj -> toHom f obj $ toHom a obj
```

So we can actually build out applicatives from a representable functor. How about monads?

``` haskell
    instance (Repr f) => Monad (Wrap f) where
      return = toF . const
      m >>= f = toF $ \obj -> ($obj) . toHom . f $ toHom m obj
```

Notice how these are working? The functor and monad are defined "pointwise".
Basically we're applying each function at a "point" in our functor's underlying
structure and then peeking at the result at that point.

If we translate this into `Forever` functor, our applicative instance would
correspond to taking a stream of functions, and zipping it with a stream of values.
Our monad instance would do the same, and select the point in the same position
in the resulting list. That's why we often refer to these as zippy monads.

Well hopefully I've convinced you that representable functors are interesting,
remember, we were able to build all of this from a simple isomorphism with `Hom`.
Cool right?
