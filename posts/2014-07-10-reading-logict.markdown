---
title: Examining Hackage: logict
---

One of my oldest habits with programming is reading other people's
code. I've been doing it almost since I started programming. For the
last two years that habit has been focused on Hackage. Today I was
reading the source code to the "logic programming monad" provided by
[`logict`][logict] and wanted to blog about how I go about reading new
Haskell code.

This time the code was pretty tiny, `find . -name *.hs | xargs wc -l`
reveals two files with just under 400 lines of code! `logict` also
only has two dependencies, base and the mtl!

## Setting Up

It's a lot easier to read this post if you have the source for logict
on hand. To grab it, use `cabal get`. My setup is something like

    ~ $ cabal get logict
    ~ $ cd logict-0.6.0.2
    ~/logict-0.6.0.2 $ cabal sandbox init
    ~/logict-0.6.0.2 $ cabal install --only-dependencies
    
## Poking Around

I'm somewhat ashamed to admit that I use pretty primitive tooling for
exploring a new codebase, it's `grep` and `find` all the way! If you
use a fancy IDE, perhaps you can just skip this section and take a
moment to sit back and feel high-tech.

First things first is to figure out what Haskell files are here. It
can be different than what's listed on Hackage since often libraries
don't export external files.

    ~/logict-0.6.0.2 $ find . -name *.hs
      ./dist/build/autogen/Paths_logict.hs
      ./Control/Monad/Logic.hs
      ./Control/Monad/Logic/Class.hs

Alright, there's two source file and one sitting in dist. The dist one
is almost certainly just cabal auto-gened stuff that we don't care
about.

It also appears that there's no `src` directory and every module is
publicly exported! This means that we only have two modules to worry
about.

The next thing to figure out is which to read first. In this case the
choice is simple: greping for imports with

    grep "import" -r Control

reveals that `Control.Monad.Logic` imports `Control.Monad.Logic.Class`
so we start with `*.Class`.

## Reading `Control.Monad.Logic.Class`

Alright! Now it's actually time to start reading code.

The first thing that jumps out is the export list

    module Control.Monad.Logic.Class (MonadLogic(..), reflect, lnot) where

Alright, so we're exporting everything from a class `MonadLogic`, as
well as two functions `reflect` and `lnot`. Let's go figure out what
`MonadLogic` is.

    class (MonadPlus m) => MonadLogic m where
      msplit     :: m a -> m (Maybe (a, m a))
      interleave :: m a -> m a -> m a
      (>>-)      :: m a -> (a -> m b) -> m b
      ifte       :: m a -> (a -> m b) -> m b -> m b
      once       :: m a -> m a

The fact that this depends on `MonadPlus` is pretty significant. Since
most classes don't require this I'm going to assume that it's fairly
key to either the implementation of some of these methods or to using
them. Similar to how `Monoid` is critical to `Writer`.

The docs make it pretty clear what each member of this class does

  - `msplit`

    Take a local computation and split it into it's first result and
    another computation that computes the rest.

  - `interleave`

    This is the key difference between `MonadLogic` and
    `[]`. `interleave` gives fair choice between two computation. This means
    that every result that appears in finitely many applications of
    `msplit` for some `a` and `b`, will appear in finitely many
    applications of `msplit` to `interleave a b`.

  - `>>-`

     `>>-` is similar to `interleave`. Consider some code like

          (a >>= k) `mplus` (b >>= k)

     This is equivalent to `mplus a b >>= k`, but has different
     characteristics since `>>=` might never terminate. `>>-` is
     described as "considering both sides of the disjunction".

     I have absolutely no idea what that means.. hopefully it'll be
     clearer once we look at some implementations.

  - `ifte`

     This is the equivalent of Prolog's soft cut. We poke a logical
     computation and if it *can* succeed at all, then we feed it into
     the success computation, otherwise we'll feed return the failure case.

  - `once`

     `once` is clever combinator to prevent backtracking. It will grab
     the first result from a computation, wrap it up and return
     it. This prevents backtracking further on the original
     computation.

Now the docs also state that everything is derivable from
`msplit`. These implementations look like

    interleave m1 m2 = msplit m1 >>=
                        maybe m2 (\(a, m1') -> return a `mplus` interleave m2 m1')

    m >>- f = do (a, m') <- maybe mzero return =<< msplit m
                 interleave (f a) (m' >>- f)

    ifte t th el = msplit t >>= maybe el (\(a,m) -> th a `mplus` (m >>= th))

    once m = do (a, _) <- maybe mzero return =<< msplit m
                return a


The first thing I notice looking at interleave is that it kinda looks
like

    interleave' :: [a] -> [a] -> [a]
    interleave' (x:xs) ys = x : interleave' ys xs
    interleave _ ys       = ys

This makes sense, since this will fairly split between `xs` and `ys`
just like `interleave` is supposed to. Here `msplit` is like pattern
matching, `mplus` is `:`, and we have to sprinkle some `return` in
there for kicks and giggles.

Now about this mysterious `>>-`, the biggest difference is that each
`f a` is `interleaved`, rather than `mplus`-ed. This should mean that
it can be fairly split between our first result, `f a` and the rest of
them `m' >>- f`. Now if we can do something like

    (m >>- f) `interleave` (m' >>- f)

Should have nice and fair behavior.

The next two are fairly clear, `ifte` splits it's computation, and if
it can it feeds the whole stinking thing `return a `mplus` m'` to the
success computation, otherwise it just returns the failure
computation. Nothing stunning.

`once` is my favorite function. To prevent backtracking all we do is
grab the first result and `return` it.

So that takes care of `MonadTrans`. The next thing to worry about are
these two functions `reflect` and `lnot`.

`reflect` confirms my suspicion that the dual of `msplit` is `mplus
(return a) m'`.

``` haskell
    reflect :: MonadLogic m => Maybe (a, m a) -> m a
    reflect Nothing = mzero
    reflect (Just (a, m)) = return a `mplus` m
```

The next function `lnot` negates a logical computation. Now, this is a
little misleading because the negated computation either produces one
value, `()`, or is `mzero` and produces nothing. This is easily
accomplished with `ifte` and `once`

``` haskell
    lnot :: MonadLogic m => m a -> m ()
    lnot m = ifte (once m) (const mzero) (return ())
```

That takes care of most of this file. What's left is a bunch of
instances for monad transformers for `MonadTrans`. There's nothing to
interesting in them so I won't talk about them here. It might be worth
glancing at the code if you're interested.

One slightly odd thing I'm noticing is that each class implements
*all* the methods, rather than just `msplit`. This seems a bit
odd.. I guess the default implementations are significantly slower?
Perhaps some benchmarking is in order.

## Control.Monad.Logic

Now that we've finished with Control.Monad.Logic.Class, let's move on
to the main file.

Now we finally see the definition of `LogicT`

``` haskell
    newtype LogicT m a =
        LogicT { unLogicT :: forall r. (a -> m r -> m r) -> m r -> m r }
```

I have no idea how this works, but I'm guessing that this is a church
version of `[a]` specialized to some `m`. Remember that the church
version of `[a]` is

``` haskell
    type CList a = forall r. (a -> r -> r) -> r -> r
```

Now what's interesting here is that the church version is strongly
connected to how CPSed code works. We could than imagine that `mplus`
works like `cons` for church lists and yields more and more
results. But again, this is just speculation.

This suspicion is confirmed by the functions to extract values out of
a `LogicT` computation

``` haskell
    observeT :: Monad m => LogicT m a -> m a
    observeT lt = unLogicT lt (const . return) (fail "No answer.")
    
    observeAllT :: Monad m => LogicT m a -> m [a]
    observeAllT m = unLogicT m (liftM . (:)) (return [])

    observeManyT :: Monad m => Int -> LogicT m a -> m [a]
    observeManyT n m
        | n <= 0 = return []
        | n == 1 = unLogicT m (\a _ -> return [a]) (return [])
        | otherwise = unLogicT (msplit m) sk (return [])
     where
     sk Nothing _ = return []
     sk (Just (a, m')) _ = (a:) `liftM` observeManyT (n-1) m'
```

`observeT` grabs the `a` from the success continuation and if no
result is returned than it will evaluate `fail "No Answer` which looks
like the failure continuation! Looks like out suspicion is confirmed,
we're dealing with monadic church lists or some other permutation of
those buzzwords.

Somehow in a package partially designed by Oleg I'm not surprised to
find continuations :)

`observeAllT` is quite similar, notice that we take advantage of the
fact that `r` is universally quantified to instantiate it to `a`. This
quantification is also used in `observeManyT`. This quantification
also prevents any `LogicT` from taking advantage of the return type to
do evil things with returning random values that happen to match the
return type. This is what's possible with `ContT` for example.

Now we have the standard specialization and smart constructor for the
non-transformer version.

``` haskell
    type Logic = LogicT Identity
    
    logic :: (forall r. (a -> r -> r) -> r -> r) -> Logic a
    logic f = LogicT $ \k -> Identity .
                             f (\a -> runIdentity . k a . Identity) .
                             runIdentity
```

Look familiar? Now we can inject real church lists into a `Logic`
computation. I suppose this shouldn't be surprising since `[a]`
functions like a slightly broken `Logic a`, without any sharing or
soft cut.

Now we repeat all the `observe*` functions for `Logic`, I'll omit
these since they're implementations are exactly as you'd expect and
not interesting.

Next we have a few type class instances

``` haskell
    instance Functor (LogicT f) where
        fmap f lt = LogicT $ \sk fk -> unLogicT lt (sk . f) fk
    
    instance Applicative (LogicT f) where
        pure a = LogicT $ \sk fk -> sk a fk
        f <*> a = LogicT $ \sk fk -> unLogicT f (\g fk' -> unLogicT a (sk . g) fk') fk
    
    instance Alternative (LogicT f) where
        empty = LogicT $ \_ fk -> fk
        f1 <|> f2 = LogicT $ \sk fk -> unLogicT f1 sk (unLogicT f2 sk fk)
    
    instance Monad (LogicT m) where
        return a = LogicT $ \sk fk -> sk a fk
        m >>= f = LogicT $ \sk fk -> unLogicT m (\a fk' -> unLogicT (f a) sk fk') fk
        fail _ = LogicT $ \_ fk -> fk
```

It helps for reading this if you expand `sk` to "success continuation"
and `fk` to "fail computation". Since we're dealing with church lists
I suppose you could also use `cons` and `nil`.

What's particularly interesting to me here is that there are *no*
constraints on `m` for these type class declarations! Let's go through
them one at a time.

`Functor` is usually pretty mechanical, and this is no exception. Here
we just have to change `a -> m r -> m r` to `b -> m r -> m r`. This is
trivial just by composing the success computation with `f`.

`Applicative` is similar. `pure` just lifts a value into the church
equivalent of a singleton list, `[a]`. `<*>` is a little bit more
meaty, we first unwrap `f` to it's underlying function `g`, and
composes it with out successes computation for `a`. Notice that this
is very similar to how `Cont` works, continuation passing style is
necessary with church representations.

Now `return` and `fail` are pretty straightforward. Though this is
interesting because since pattern matching calls `fail`, we can just
do something like

    do
      Just a <- m
      Just b <  n
      return $ a + b

And we'll run `n` and `m` until we get a `Just` value.

As for `>>=`, it's implementation is very similar to `<*>`. We unwrap
`m` and then feed the unwrapped `a` into `f` and run that with our
success computations.

We're only going to talk about one more instance for `LogicT`,
`MonadLogic`, there are a few others but they're mostly for MTL use
and not too interesting.

    instance (Monad m) => MonadLogic (LogicT m) where
        msplit m = lift $ unLogicT m ssk (return Nothing)
         where ssk a fk = return $ Just (a, (lift fk >>= reflect))

We're only implementing `msplit` here, which strikes me as a bit odd
since we implemented everything before. We also actually need `Monad m`
here so that we can use `LogicT`'s `MonadTrans` instance.

To split a `LogicT`, we run a special success computation and return
`Nothing` if failure is ever called. Now there's one more clever trick
here, since we can choose what the `r` is in `m r`, we choose it to be
`Maybe (a, LogicT m a)`! That way we can take the failure case, which
essentially is just the tail of the list, and push it into `reflect`.

This confused me a bit so I wrote the equivalent version for church
lists, where `msplit` is just `uncons`.

``` haskell
    {-# LANGUAGE RankNTypes #-}
    
    newtype CList a = CList {runCList :: forall r. (a -> r -> r) -> r -> r}
    
    cons :: a -> CList a -> CList a
    cons a (CList list) = CList $ \cs nil -> cs a (list cs nil)
    
    nil :: CList a
    nil = CList $ \cons nil -> nil
    
    head :: CList a -> Maybe a
    head list = runCList list (const . Just) Nothing
    
    uncons :: CList a -> Maybe (a, CList a)
    uncons (CList list) = list skk Nothing
      where skk a rest = Just (a, maybe nil (uncurry cons) rest)
```

Now it's a bit clearer what's going on, `skk` just pairs up the head
of the list with the rest. However, since the tail of the list has the
type `m (Maybe (a, LogicT m a))`, we lift it back into the `LogicT`
monad and use `reflect` to smush it back into a good church list.

That about covers Control.Monad.Logic

## Wrap Up

I've never tried sharing these readings before so I hope you enjoyed
it. If this receives some positive feedback I'll do something similar
with another package, I'm leaning towards extensible-effects.

If you're interested in doing this yourself, I highly recommend it!
I've learned a *lot* about practical engineering with Haskell, as well
as really clever and elegant Haskell code.

One thing I've always enjoyed about the Haskell ecosystem is that some
of the most interesting code is often quite easy to read given some time.


[logict]: http://hackage.haskell.org/package/logict
