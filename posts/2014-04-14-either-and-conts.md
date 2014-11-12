---
title: Continuations and Exceptions
tags: haskell
---

Continuations are useful things. They provide a nice way to manually
handle control flow. This fact makes them very useful for compilers,
an often used alternative to SSA is an intermediate language in which
every function call is a tail-call and every expression has been
converted to continuation passing style.

Often however, this isn't enough. In a language which exceptions, we
don't just have a single continuation. Since every expression can
either do one of two things.

 1. Continue the rest of the program normally
 2. Throw an exception and run an alternative program, the exception
    handler

To represent this, we can imagine having two continuations. Instead of

``` haskell
    newtype Cont r a = Cont {runCont :: (a -> r) -> r}
```

We have

``` haskell
    {-# LANGUAGE DeriveFunctor #-}
    import Control.Monad

    newtype Throws r e a = Throws {runThrows :: (e -> r) -> (a -> r) -> r}
    deriving (Functor)
```

Now we have two continuations, where `e -> r` represents the
composition of exception handlers.

We can write a trivial monad instance similar to `Cont`

``` haskell
    instance Monad (Throws r e) where
      return a = Throws $ \ex cont -> cont a
      (Throws c) >>= f = Throws $ \ ex cont ->
        c ex $ \a -> runThrows (f a) e cont
```

So `>>=` maintains the exception handler between computations and
otherwise acts exactly like `Cont`.

To actually take advantage of our exception handlers, we need two
things, a `throw` and `catch` like pair of function. Let's start with
`throw` since it's easiest.

``` haskell
    throw :: e -> Throws r e a
    throw e = Throws $ \ex cont -> ex e
```

This is pretty straightforward, when we're given an exception an to
throw, we simply feed it to our exception handler continuation. Since
care what value `cont` needs, we can universally quantify over `a`.


Next up is `handle`, we'll represent an exception handler as a
function from `e -> Maybe a`. If we return `Nothing`, we can't handle
the exception at this level and we'll just pass it to the existing
exception handler.

So our `handle` is

``` haskell
    handle :: Throws r e a -> (e -> Maybe a) -> Throws r e a
    handle (Throws rest) handler = Throws $ \ex cont ->
      rest (\e -> maybe (ex e) cont (handler e)) cont
```

Notice the clever bit here, each handler actually contains both the
success and failure continuations! If we can't handle the exception we
fail otherwise we can resume exactly where we were before.

No post would be complete without a demonstration!

``` haskell
    data Ex = Boom | Kaboom | Splat String
            deriving Show

    throwEx1 = throw Boom
    throwEx2 = throw Kaboom
    throwEx3 = throw (Splat "splat")
    test = do
      result <- handle throwEx1 $ \e -> case e of
        Boom -> Just "foo"
        _    -> Nothing
      result2 <- handle throwEx2 $ \e -> case e of
        Boom   -> Just "what"
        Kaboom -> Just "huh?"
        _      -> Nothing
      result3 <- handle throwEx3 $ \e -> case e of
        Splat s -> Just s
        _       -> Nothing
      return (unwords [result, result2, result3])
```

We can run this with

``` haskell
    runThrows (error . ("Toplevel fail "++)) test
```

which returns

``` haskell
    "foo huh? splat"
```

So our exceptions do in fact, work :)
## A Note on `Either`
Now we already have a perfectly good system of monadic exception like
thing in the form of `Either`.

It might be interesting to note that what we've written is in fact
isomorphic to `Either`. `(e -> r) -> (a -> r) -> r` is just the church
representation of `Either e a`.

We can even go all the way and change `Throws` to

``` haskell
    newtype Throws e a = Throws {runThrows :: forall r. (e -> r) -> (a -> r) -> r}
```

So there you are, an interesting realization that one of the classic
representations of a language like SML is in fact a really complicated
version of `Either` :)
