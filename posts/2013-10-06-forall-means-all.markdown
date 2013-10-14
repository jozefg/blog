---
title: forall Means All!
---
It seems like the every week on Stack Overflow there's at least two questions
about higher rank polymorphism (RankNTypes). So here's a brief description
of what they are and how to use them.


First, to turn them on


    {-# LANGUAGE RankNTypes #-}

Now to write one


     demo :: (forall a. a -> Int) -> Int
     demo f = f 'a' + f True

Now that `forall` means "this function is polymorphic and can be applied
to any argument". Notice we can't do this without rank N types,

    uhoh :: (a -> Int) -> Int
    uhoh f = f 'a' + f True -- Error cannot unify 'a' with Char

Here's how not to use it

    demo id

`id` here unifies with `Int -> Int` which isn't the necessary
`forall a. a -> Int`. To use it,

    demo (const 1)

Now this seems pretty clear right? It's just to make it possible to
pass polymorphic functions to other functions. Easy peasy :)

Now what does this mean?


    data Tricky = Tricky (forall x. x -> x)

Well you'd be right if you realized that the only sane instantiation of this
is

    t = Tricky id

We need a function

    arg :: a -> a

It's pretty clear that the only sane version of `arg` is `id`.

A harder one,

    type ReallyTricky a b = forall f. Functor f => (a -> b) -> f a -> f b

That's right it just means that anything of type `ReallyTricky` knows how to
take some arbitrary function, and lift it into *any* functor. And the caller gets
to choose which one.

    t :: ReallyTricky
    t = fmap

That's it! Just remember that `forall` is universal quantification. This means
that you have to be able to support all possible instationations of that variable and
the caller will choose which one.

Now suppose you want it the other way, you choose the instantiation and the caller
has to handle it generically. Then you want `existential` quantification. A subject for
another post.
