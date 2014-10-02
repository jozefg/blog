---
title: Notes on Abstract and Existential Types
tags: haskell, types
---

I'm part of a paper reading club at CMU. Last week we talked about a
classic paper, [Abstract Types have Existential Type][paper]. The
concept described in this paper is interesting and
straightforward. Sadly some of the notions and comparisons made in the
paper are starting to show their age. I thought it might be fun to
give a tldr using Haskell.

The basic idea is that when we have an type with an abstract
implementation some functions upon it, it's really an existential
type.

## Some Haskell Code

To exemplify this let's define an abstract type (in Haskell)

``` haskell
    module Stack (Stack, empty, push, pop) where
    newtype Stack a = Stack [a]

    empty :: Stack a
    empty = Stack []

    push :: a -> Stack a -> Stack a
    push a (Stack xs) = Stack (a : xs)

    pop :: Stack a -> Maybe a
    pop (Stack []) = Nothing
    pop (Stack (x : xs)) = Just x

    shift :: Stack a -> Maybe (Stack a)
    shift (Stack []) = Nothing
    shift (Stack (x : xs)) = Just (Stack xs)
```

Now we could import this module and use its operations:

``` haskell
    import Stack

    main = do
      let s = push 1 . push 2 . push 3 $ empty
      print (pop s)
```

What we couldn't do however, is pattern match on stacks to take
advantage of its internal structure. We can only build new operations
out of combinations of the exposed API. The classy terminology would
be to say that `Stack` is abstract.

This is all well and good, but what does it *mean* type theoretically?
If we want to represent Haskell as a typed calculus it'd be a shame to
have to include Haskell's (under powered) module system to talk about
abstract types.

After all, we're not really thinking about modules as so much as
hiding some details. That sounds like something our type system should
be able to handle without having to rope in modules. By isolating the
concept of abstraction in our type system, we might be able to more
deeply understand and reason about code that uses abstract types.

This is in fact quite possible, let's rephrase our definition of
`Stack`

``` haskell
    module Stack (Stack, StackOps(..), ops) where

    newtype Stack a = Stack [a]

    data StackOps a = StackOps { empty :: Stack a
                               , push  :: a -> Stack a -> Stack a
                               , pop   :: Stack a -> Maybe a
                               , shift :: Stack a -> Maybe (Stack a) }
    ops :: StackOps
    ops = ...
```

Now that we've lumped all of our operations into one record, our
module is really only exports a type name, and a record of data. We
could take a step further still,

``` haskell
    module Stack (Stack, StackOps(..), ops) where

    newtype Stack a = Stack [a]

    data StackOps s a = StackOps { empty :: s a
                                 , push  :: a -> s a -> s a
                                 , pop   :: s a -> Maybe a
                                 , shift :: s a -> Maybe (s a) }
    ops :: StackOps Stack
    ops = ...
```

Now the only thing that needs to know the internals of `Stack`. It
seems like we could really just smush the definition into `ops`, why
should the rest of the file see our private definition.

``` haskell
    module Stack (StackOps(..), ops) where

    data StackOps s a = StackOps { empty :: s a
                                 , push  :: a -> s a -> s a
                                 , pop   :: s a -> Maybe a
                                 , shift :: s a -> Maybe (s a) }
    ops :: StackOps ???
    ops = ...
```

Now what should we fill in `???` with? It's some type, but it's meant
to be chosen by the callee, not the caller. Does that sound familiar?
Existential types to the rescue!

``` haskell
    {-# LANGUAGE PolyKinds, KindSignatures, ExistentialQuantification #-}
    module Stack where

    data Packed (f :: k -> k' -> *) a = forall s. Pack (f s a)

    data StackOps s a = StackOps { empty :: s a
                                 , push  :: a -> s a -> s a
                                 , pop   :: s a -> Maybe a
                                 , shift :: s a -> Maybe (s a) }
    ops :: Packed StackOps
    ops = Pack ...
```

The key difference here is `Packed`. It lets us take a type function
and instantiate it with some type variable and hide our choice from
the user. This means that we can even drop the whole `newtype` from
the implementation of `ops`

``` haskell
    ops :: Packed StackOps
    ops = Pack $ StackOps { empty = []
                          , push  = (:)
                          , pop   = fmap fst . uncons
                          , shift = fmap snd . uncons }
      where uncons [] = Nothing
            uncons (x : xs) = Just (x, xs)
```

Now that we've eliminated the `Stack` definition from the top level, we
can actually just drop the notion that this is in a separate module.

One thing that strikes me as unpleasant is how `Packed` is defined, we
must jump through some hoops to support `StackOps` being polymorphic
in two arguments, not just one.

We could get around this with higher rank polymorphism and making the
*fields* more polymorphic while making the type less so. We could also
just wish for type level lambdas or something. Even some of the recent
type level lens stuff could be aimed at making a general case
definition of `Packed`.

From the client side this definition isn't actually so unpleasant to
use either.

``` haskell
    {-# LANGUAGE RecordWildCards #-}

    someAdds :: Packed Stack Int -> Maybe Int
    someAdds (Pack Stack{..}) = pop (push 1 empty)
```

With record wild cards, there's very little boilerplate to introduce
our record into scope.  Now we might wonder about using a specific
instance rather than abstracting over all possible instantiations.

``` haskell
    someAdds :: Packed Stack Int -> Maybe Int
    someAdds =
      let (Pack Stack{..}) = ops in
        pop (push 1 empty)
```

The resulting error message is amusing :)

Now we might wonder if we gain anything concrete from this. Did all
those language extensions actually do something useful?

Well one mechanical transformation we can make is that we can change
our existential type into a CPS-ed higher rank type.

``` haskell
    unpackPacked :: (forall s. f s a -> r) -> Packed f a -> r
    unpackPacked cont (Pack f) = cont f

    someAdds' :: Stack s Int -> Maybe Int
    someAdds' Stack{..} = pop (push 1 empty)

    someAdds :: Packed Stack Int -> Maybe Int
    someAdds = unpackPacked someAdds'
```

Now we've factored out the unpacking of existentials into a function
called `unpack`. This takes a continuation which is parametric in the
existential variable, `s`.

Now our body of `someAdds` becomes `someAdds`, but notice something
very interesting here, now `s` is a normal universally quantified type
variable. This means we can apply some nice properties we already have
used, eg parametricity.

This is a nice effect of translating things to core constructs, all
the tools we already have figured out can suddenly be applied.

## Wrap Up

Now that we've gone through transforming our abstract types in
existential ones you can final appreciate at least one more thing: the
subtitle on [Bob Harper's blog][bobs blog]. You can't say you didn't
learn something useful :)

I wanted to keep this post short and sweet. In doing this I'm going to
some of the more interesting questions we could ask. For the curious
reader, I leave you with these

 - How can we use type classes to prettify our examples?
 - What can we do to generalize `Packed`?
 - How does this pertain to modules? Higher order modules?
 - How would you implement "sharing constraints" in this model?
 - What happens when we translate existentials to dependent products?

Cheers.

[paper]: http://theory.stanford.edu/~jcm/papers/mitch-plotkin-88.pdf
[bobs blog]: http://existentialtype.wordpress.com/
