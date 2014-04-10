---
title: Bargain Priced Coroutines
---

The other day I was reading the 19th issue of the Monad.Reader
and there was a fascinating post on coroutines.

While reading some of the code I noticed that it, like
most things in Haskell, can be reduced to 5 lines with
a library that Edward Kmett has written.

Consider the type of a trampoline as described in this article

``` haskell
    newtype Trampoline m a = Tramp {runTramp :: m (Either (Tramp m a) a)}
```

So a trampoline is a monadic computation of some sort returning either
a result, `a`, or another computation to run to get the rest.

Now this looks strikingly familiar. A computation returning `Trampoline m a`
is really a computation returning a tree of `Tramp m a`'s terminating in a pure
value.

This sounds like a free monad!

``` haskell
    import Control.Monad.Trans.Free
    import Control.Monad.Identity
    
    type Trampoline = FreeT Identity
```

Recall that `FreeT` is defined as

``` haskell
    data FreeF f a b = Pure a | Free (f b)
    data FreeT f m a = FreeT (m (FreeF f a (FreeT f m a)))
```

This is isomorphic to what we where looking at before. As an added bonus,
we've saved the tedium of defining our own monad and applicative instance
for `Trampoline`.

We can now implement `bounce` and `pause` to define our trampolines.
`bounce` must take a computation and unwrap it by one level, leaving
either a value or another computation.

This is just a matter of rejiggering the `FreeF` into an `Either`

``` haskell
    bounce :: Functor m => Trampoline m a -> m (Either (Trampoline m a) a)
    bounce = fmap toEither . runFreeT
      where toEither (Pure a) = Right a
            toEither (Free m) = Left $ runIdentity m
```
`pause` requires some thought, the trick is to realize that if we wrap a computation in
one layer of `Free` when unwrapped by `bounce` we'll get the rest of the computation.

Therefore,

``` haskell
    pause :: Monad m => Trampoline m ()
    pause = FreeT $ return (Free . Identity $ return ())
```

So that's 6 lines of code for trampolines. Let's move on to generators.

A generator doesn't yield just another computation, it yields a pair of a
computation and a freshly generated value. We can account for this by changing
that `Identity` functor.

``` haskell
    type Generator c = FreeT ((,) c)
```

Again we get free functor, applicative and monad instances. We two
functions, `yield` and `runGen`. Yield is going to take one value and
stick it into the first element of the pair.

``` haskell
    yield :: Monad m => g -> Generator g m ()
    yield g = FreeT . return $ Free (g, return ())
```

This just sticks a good old boring `m ()` in the second element
of the pair.

Now `runGen` should take a generator and produce a `m (Maybe c, Generator c m a)`.
This can be done again by pattern matching on the underlying `FreeF`.

``` haskell
    runGen :: (Monad m, Functor m) => Generator g m a -> m (Maybe g, Generator g m a)
    runGen = fmap toTuple . runFreeT
      where toTuple (Pure a)         = (Nothing, return a)
            toTuple (Free (g, rest)) = (Just g, rest)

```

Now, last but not least, let's build consumers. These wait for a value rather
than generating one, so `->` looks like the right functor.

``` haskell
    type Consumer c = FreeT ((->) c)
```

Now we want `await` and `runCon`. `await` to wait for a value
and `runCon` to supply one. These are both fairly mechanical.


``` haskell
    runConsumer :: Monad m => c -> Consumer c m a -> m a
    runConsumer c = (>>= go) . runFreeT
      where go (Pure a) = return a
            go (Free f) = runConsumer c $ f c

    runCon :: (Monad m, Functor m)
        => Maybe c
        -> Consumer c m a
        -> m (Either a (Consumer c m a))
    runCon food c = runFreeT c >>= go
      where go (Pure a) = return . Left $ a
            go (Free f) = do
              result <- runFreeT $ f food
              return $ case result of
                Pure a -> Left                   $ a
                free   -> Right . FreeT . return $ free
```
`runCon` is a bit more complex than I'd like. This is to essentially ensure that if we had some code
like

``` haskell
    Just a <- await
    lift $ do
      foo
      bar
      baz
    Just b <- await
```

We want `foo`, `bar`, and `baz` to run with just one `await`. You'd expect that we'd run
as much as possible with each call to `runCon`. Thus
we unwrap not one, but two layers of our `FreeT` and run them, then rewrap the lower
layer. The trick is that we make sure never to duplicate side effects by using good old `return`.

We can sleep easy that this is sound since `return a >>= f` is `f a` by the monad laws. Thus, our
call to `return` can't do anything detectable or too interesting.

While this is arguably more intuitive, I don't particularly like it so we can instead
write

``` haskell
    runCon :: (Monad m, Functor m)
        => Maybe c
        -> Consumer c m a
        -> m (Either a (Consumer c m a))
    runCon food = fmap go . runFreeT
      where go (Pure a) = Left a
            go (Free f) = Right (f food)
```

Much simpler, but now our above example wouldn't run `foo` and friends until the *second*
call of `runCon`.

Now we can join generators to consumers in a pretty naive way,

``` haskell
    (>~>) :: (Functor m, Monad m) => Generator c m () -> Consumer c m a -> m a
    gen >~> con = do
      (cMay, rest) <- runGen gen
      case cMay of
        Nothing -> starve con
        Just c  -> runCon c con >>= use rest
      where use _    (Left a)  = return a
            use rest (Right c) = rest >~> c
```

And now we can use it!

``` haskell
    addGen :: Generator Int IO ()
    addGen = do
      lift $ putStrLn "Yielding 1"
      yield 1
      lift $ putStrLn "Yielding 2"
      yield 2
    
    addCon :: Consumer Int IO ()
    addCon = do
      lift $ putStrLn "Waiting for a..."
      Just a <- await
      lift $ putStrLn "Waiting for b..."
      Just b <- await
      lift . print $ a + b

    main = addGen >~> addCon
```
When run this prints

```
    Yielding 1
    Waiting for a...
    Yielding 2
    Waiting for b...
    3
```

Now, this all falls out of playing with what functor we give to `FreeT`.
So far, we've gotten trampolines out of `Identity`, generators out of `(,) a`,
and consumers out of `(->) a`.
