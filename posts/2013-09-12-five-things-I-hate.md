-----
title: Logic and Continuations
tags: haskell
-----

In Haskell there's a monad known as `Cont`. Most people don't use it, but it's pretty cool. The basic idea is that

``` haskell
     Cont r a = Cont {runCont :: (a -> r) -> r}
```

The intuition being that that `(a -> r)` term is the "rest of the program". You feed it the type it is expecting
and it will happily run the rest of your computation.

### Deriving Monad (Cont r)

`return` is easy

``` haskell
    return a = Cont ($a)
```

or

``` haskell
    return a = Cont $ \c -> c a
```

Bind is a little trickier

``` haskell
    (Cont c) >>= f = Cont (\rest -> c $ \a -> runCont (f a) rest)
```

Think of this as feeding the continuation `c :: (a -> r) -> r` a function made by
with a continuation `runCont . f :: a -> (b -> r) -> r` and `rest :: (b -> r)`
to make something of type `a -> r`.

Yeah it hurts your head a little.

Now let's talk about seeing into the future.

### Back to the Future

First things first, to conform with how the MTL does stuff

``` haskell
     cont = Cont
```

Because in the real `Control.Monad.Cont`, there's a monad transformer and a
type synonym

``` haskell
     type Cont r a = ContT r Identity a
```

or something like that.

Let's try a simple application of `Cont`. Suppose we have a function
of type `[a -> Bool] -> [a] -> [a]` and we want to return the longest
list of `a`s that satisfies one of the predicates in our list. Yeah it's
contrived but I'll show you a more realistic example in a second:

``` haskell
     longest preds as = do
       p <- selectPred preds
       return (filter p as)
```

Now we just need to define `selectPred` so that it can "know" which predicate will return the longest list

``` haskell
    selectPred ps = cont $ \c -> maxBy length . map c $ ps
```

That's it. It's actually running the program with each possible value and then returns the best result. Cool right?

Note that it's kind of important that things are pure here. If you have an `unsafePerformIO` and you start backtracking
things get hairy. However, since you can toss around `IO`s without evaluating them, eg

``` haskell
     const 1 (print "foo")
```

Doesn't print `foo` or anything, you can have `ContT` layered over IO.

#### Logic Framework

Now let's use this to create a simple framework for non-deterministic logic programming. Some skeleton code:

``` haskell
    {-# LANGUAGE RankNTypes #-}
    newtype Logic a = Logic {runLogic :: forall r.  Cont (Maybe r) a}
    instance Monad Logic where
      return a = Logic $ return a
      (Logic c) >>= f  = Logic $ c >>= runLogic . f
```

The `RankNTypes` basically says a logical computation can't make assumptions about the result, which is pretty reasonable.
The monad instance is just relying on the underlying `Cont` instance. Now we want three functions:

``` haskell
    amb :: [a] -> Logic a
    disconj :: Logic a -> Logic a -> Logic a
    backtrack :: Logic ()
```

where `backtrack` backtracks to the nearest `amb` and tries the next element and `disconj` simply joins
together two propositions and chooses an element from one that won't fail (return `Nothing`).

``` haskell
    backtrack = Logic (cont $ const Nothing)
    disconj (Logic a) (Logic b) = Logic (cont $ \c -> runCont a c `mplus` runCont b c)
    amb as = Logic (cont $ \c -> join . find isJust . map (c$) $ as)
```

Note: with RankNTypes weird things can happen with `.` for example, using `Logic . cont` is ill typed presumably
because GHC restricts the `Cont` being fed to `Logic`.

Now these actually map nicely to an existing typeclass.

``` haskell
     instance MonadPlus Logic where
       mzero = backtrack
       mplus = disconj
```

Also helpful is

``` haskell
    evaluate :: Logic a -> Maybe a
    evaluate = flip runCont Just . runLogic
```

So let's try it out:

``` haskell
     main = print . evaluate $ do
       a <- amb [1, 2, 3] :: Logic Integer
       b <- amb [4, 5, 6]
       when (a + b /= 9) mzero
       return (a, b)
```

and perhaps a helpful combinator

``` haskell
     assert = flip when mzero
```

makes

``` haskell
     main = print . evaluate $ do
       a <- amb "floor"
       b <- amb "bar"
       assert (a==b)
       return (a, b)
```

And there you have it, using continuations we have created a logic DSL. The interesting
bit is that each `amb` is actually running the code multiple times and "seeing into the future".
Once it has which element will actually return a desirable result it pops it back. Nifty.

### An exercise to the reader

A useful exercise is to add 1 of 2 combinators

``` haskell
    cut :: Logic ()
    interleave :: Logic a -> Logic b -> Logic (a, b)
```

`cut` doesn't backtrack. To implement this, you'll have to use `callCC` and pass the escape continuation around
and call it from `cut` if something tries to backtrack past it.

`interleave` is also cool, it's fair disjunction. Our current setup can't handle

``` haskell
     a <- amb [1, 2, 3, 4, 5]
     b <- amb [1..]
     assert ( a == 2 )
```

It'll get stuck fiddling with the value of `b`! With `interleave` we'd type

``` haskell
     (a, b) <- interleave (amb [1, 2, 3]) (amb [1..])
```

and it will give us pairs "fairly" by returning pairs so that the probability of `(n, m)` being returned
when `n` is `a` elements into the first computation and `m` is `b` elements into the second is `k / (a + b)`.

Good luck!
