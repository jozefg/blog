---
title: Examining Hackage: operational
tags: haskell
---

In this installment of "jozefg is confused by other people's code" we
turn to `operational`. This is a package that's a little less known
than I'd like. It provides a monad for transforming an ADT of
instructions a monad that can be used with `do` notation and separates
out interpenetration.

Most people familiar with free monads are wondering what the
difference is between operational's approach and using
[free monads][you-could-have]. Going into this, I have no
clue. Hopefully this will become clear later on.

## Diving Into The Source

Let's get started shall we

    ~$ cabal get operational

Happily enough, there's just one (small) file so we'll go through
that.

To start with `Control.Monad.Operational` exports

``` haskell
    module Control.Monad.Operational (
        Program, singleton, ProgramView, view,
        interpretWithMonad,
        ProgramT, ProgramViewT(..), viewT,
        liftProgram,
        ) where
```

Like with most "provides a single monad" packages, I'm most interested
in how `Program` works. Looking at this, we see that it's just a synonym

``` haskell
    type Program instr = ProgramT instr Identity
```

Just like the mtl, this is defined in terms of a it's transformer. So
what's this transformer?

``` haskell
    data ProgramT instr m a where
        Lift   :: m a -> ProgramT instr m a
        Bind   :: ProgramT instr m b -> (b -> ProgramT instr m a)
               -> ProgramT instr m a
        Instr  :: instr a -> ProgramT instr m a
```

So `ProgramT` is a GADT, this is actually important because `Bind` has
an existential type variable: `b`. Otherwise this is really just a
plain tree, I assume `(>>=) = Bind` and `return = Lift . return` in
the monad instance for this. And finally we can see that instructions
are also explicitly supported with `Instr`.

We can confirm that the `Monad` instance is as boring as we'd expect
with

``` haskell
    instance Monad m => Monad (ProgramT instr m) where
        return = Lift . return
        (>>=)  = Bind

    instance MonadTrans (ProgramT instr) where
        lift   = Lift

    instance Monad m => Functor (ProgramT instr m) where
        fmap   = liftM

    instance Monad m => Applicative (ProgramT instr m) where
        pure   = return
        (<*>)  = ap
```

So clearly there's no interesting computation happening here. Looking
at the export list again, we see that there's a helpful combinator
`singleton` for building up these `Program[T]`s since they're kept
abstract.

``` haskell
    singleton :: instr a -> ProgramT instr m a
    singleton = Instr
```

Which once again is very boring.


So this is a lot like free monads it seems since neither one of these
actually does much in its monad instance. Indeed the equivalent with
free monads would be

``` haskell
    data Free f a = Pure a | Free (f (Free f a))
    instance Functor f => Monad (Free f) where
      return = Pure
      Pure a >>= f = f a
      (Free a) >>= f = Free (fmap (>>= f) a)

    singleton :: Functor f => f a -> Free f a
    singleton = Free . fmap Pure
```

The obvious differences is that

 1. `Free` requires a functor while `Program` doesn't
 2. `Free`s monad instance automatically guarantees laws

2 is the bigger one for me. `Free` has a tighter set of constraints on
its `f` so it can guarantee the monad laws. This is clearly false with
`Program` since `return a >>= f` introduces an extra `Bind` instead of
just giving `f a`.

This would explain why `ProgramT` is kept abstract, it's hopelessly
broken just to expose it in its raw form. Instead what we have to do
is somehow partially normalize it before we present it to the user.

Indeed that's exactly what `ProgramViewT` is representing. It's a
simpler data type

``` haskell
    data ProgramViewT instr m a where
        Return :: a -> ProgramViewT instr m a
        (:>>=) :: instr b
               -> (b -> ProgramT instr m a)
               -> ProgramViewT instr m a
```

This apparently "compiles" a `Program` so that everything is either
binding an instruction or a pure value. What's interesting is that
this seems to get rid of all `Lift`'s as well.

How do we produce one of these? Well that seems to be `viewT`'s job.

``` haskell
    viewT :: Monad m => ProgramT instr m a -> m (ProgramViewT instr m a)
    viewT (Lift m)                = m >>= return . Return
    viewT ((Lift m)     `Bind` g) = m >>= viewT . g
    viewT ((m `Bind` g) `Bind` h) = viewT (m `Bind` (\x -> g x `Bind` h))
    viewT ((Instr i)    `Bind` g) = return (i :>>= g)
    viewT (Instr i)               = return (i :>>= return)
```

Note that this function returns an `m (ProgramViewT instr m a)`, not
just a plain `ProgramViewT`. This makes sense because we have to get
rid of the lifts. What I think is particularly interesting here is
that the 2nd and 3rd cases are just the monad laws!

The second one says binding to a computation is just applying the
function to it in the obvious manner. The third re-associates bind in
a way guaranteed by the monad laws.

This means that while `ProgramT` isn't going to satisfy the monad
laws, we can't tell because all the things said to be equal by the
monad laws will compile to the same view. Terribly clever stuff.

The rest of the module is mostly boring stuff like `Monad*`
instances. The last interesting functions is `interpretWithMonad`

``` haskell
    interpretWithMonad :: forall instr m b.
        Monad m => (forall a. instr a -> m a) -> (Program instr b -> m b)
    interpretWithMonad f = eval . view
        where
        eval :: forall a. ProgramView instr a -> m a
        eval (Return a) = return a
        eval (m :>>= k) = f m >>= interpretWithMonad f . k
```

This nicely highlights how to you're supposed to write an interpreter
for a `Program`. `eval` handles the two cases of the `view` using the
mapping to a monad we provided and `view` handles actually compiling
the program into these two cases. All in all, not too shabby.

## Surprise, There Were Docs The Whole Time!

Now I assume that most people didn't actually download the source to
operational, but you really should! Inside you'll find a whole
directory, `doc`. It contains a few markdown files with explanations
and references to the appropriate papers as well as a couple examples
of actually building things with `operational`.

Now that you understand how the current implementation works, you
should be able to understand most of what is being said there.

## Wrap Up

So `operational` illustrates a neat trick I rather like: using
modularity to provide an `O(1)` implementation of `>>=` and hide its
rule breaking with a view.

This package also drops the positivity requirement that `Free` implies
with its functor constraint. Which I suppose means you could have

``` haskell
    data Foo a where
      Bar :: (a -> ...) -> Bar a
```

Which is potentially useful.

Last but not least, `operational` really exemplifies having a decent
amount of documentation even though there's only ~100 lines of code. I
think the ratio of documentation : code is something like 3 : 1 which
I really appreciate.

[you-could-have]: http://www.haskellforall.com/2012/06/you-could-have-invented-free-monads.html
