---
title: Examining Hackage: extensible-effects
---

I had a few people tell me after my [last post][last-post] that they would enjoy a
write up on reading [extensible-effects][ee-pack] so here goes.

I'm going to document my process of reading through and understanding
how extensible-effects is implemented. Since this is a fairly large
library (about 1k) of code, we're not going over *all* of it. Rather
we're just reviewing the core modules and enough of the extra ones to
get a sense for how everything is implemented.

If you're curious or still have questions, the modules that we don't
cover should serve as a nice place for further exploration.

## Which Modules

extensible-effects comes with quite a few modules, my find query
reveals

    $ find src -name "*.hs"
      src/Data/OpenUnion1.hs
      src/Control/Eff/Reader/Strict.hs
      src/Control/Eff/Reader/Lazy.hs
      src/Control/Eff/Fresh.hs
      src/Control/Eff/Cut.hs
      src/Control/Eff/Exception.hs
      src/Control/Eff/State/Strict.hs
      src/Control/Eff/State/Lazy.hs
      src/Control/Eff/Writer/Strict.hs
      src/Control/Eff/Writer/Lazy.hs
      src/Control/Eff/Coroutine.hs
      src/Control/Eff/Trace.hs
      src/Control/Eff/Choose.hs
      src/Control/Eff/Lift.hs
      src/Control/Eff.hs
      src/Control/Eff/Reader/Strict.hs

Whew! Well I'm going to take a leap and assume that extensible-effects
is similar to the mtl in the sense that there are a few core modules,
an then a bunch of "utility" modules. So there's `Control.Monad.Trans`
and then `Control.Monad.State` and a bunch of other implementations of
`MonadTrans`.

If we assume extensible-effects is formatted like this, then we need
to look at

  1. Data.OpenUnion1
  2. Control.Monad.Eff

And maybe a few other modules to get a feel for how to use these
two. I've added `Data.OpenUnion1` because it's imported by
`Control.Monad.Eff` so is presumably important.

Since `Data.OpenUnion1` is at the top of our dependency DAG, we'll
start with it.

## Data.OpenUnion1

So we're starting with Data.OpenUnion1. If the authors of this code
have stuck to normal Haskell naming conventions, that's an open union
of type *constructors*, stuff with the kind `* -> *`.

Happily, this module has an export list so we can at least see what's
public.

``` haskell
    module Data.OpenUnion1( Union (..)
                          , SetMember
                          , Member
                          , (:>)
                          , inj
                          , prj
                          , prjForce
                          , decomp
                          , unsafeReUnion
                          ) where
```

So we're looking at a data type `Union`, which we export everything
for. Two type classes `SetMember` and `Member`, a type operator `:>`,
and a handful of functions, most likely to work with `Union`.

So let's figure out exactly what this union thing is

    data Union r v = forall t. (Functor t, Typeable1 t) => Union (t v)

So `Union r v` is just a wrapper around some of functor applied to
`v`. This seems a little odd, what's this `r` thing? The docs hint
that `Member t r` should always hold.

`Member` is a type class of two parameters with no members. In fact,
`grep`ing the entire source reveals that the entire definition and
instances for `Member` in this code base is

``` haskell
    infixr 1 :>
    data ((a :: * -> *) :> b)
    
    class Member t r
    instance Member t (t :> r)
    instance Member t r => Member t (t' :> r)
```

So this makes it a bit clearer, `:>` acts like a type level cons and
`Member` just checks for membership!

Now `Union` makes a bit more sense, especially in light of the `inj`
function

``` haskell
    inj :: (Functor t, Typeable1 t, Member t r) => t v -> Union r v
    inj = Union
```

So `Union` takes some `t` in `r` and hides it away in an existential
applied to `v`. Now this is kinda like having a great nested bunch of
`Either`s with every `t` applied to `v`.

Dual to `inj`, we can define a projection from a `Union` to some `t`
in `r`. This will need to return something wrapped in `Maybe` since we
don't know which member of `r` our `Union` is wrapping.

``` haskell
    prj :: (Typeable1 t, Member t r) => Union r v -> Maybe (t v)
    prj (Union v) = runId <$> gcast1 (Id v)
```

`prj` does some evil `Typeable` casts, but this is necessary since
we're throwing away all our type information with that
existential. That `Id` `runId` pair is needed since `gcast1` has the
type

``` haskell
    -- In our case, `c ~ Id`
    gcast1 :: (Typeable t', Typeable t) => c (t a) -> Maybe (c (t' a))
```

They're just defined as

``` haskell
    newtype Id a = Id { runId :: a }
      deriving Typeable
```

so just like `Control.Monad.Identity`.

Now let's try to figure out what this `SetMember` thing is.

``` haskell
    class Member t r => SetMember set (t :: * -> *) r | r set -> t
    instance SetMember set t r => SetMember set t (t' :> r)
```

This is unhelpful, all we have is the recursive step with no base
case! Resorting to grep reveals that our base case is defined in
`Control.Eff.Lift` so we'll temporarily put this class off until then.

Now the rest of the file is defining a few functions to operate over
`Union`s.

First up is an unsafe "forced" version of `prj`.

``` haskell
    infixl 4 <?>

    (<?>) :: Maybe a -> a -> a
    Just a <?> _ = a
    _ <?> a = a
    
    prjForce :: (Typeable1 t, Member t r) => Union r v -> (t v -> a) -> a
    prjForce u f = f <$> prj u <?> error "prjForce with an invalid type"
```

`prjForce` is really exactly what it says on the label, it's a version
of `prj` that throws an exception if we're in the wrong state of
`Union`.

Next is a way of unsafely rejiggering the type level list that `Union`
is indexed over.

``` haskell
    unsafeReUnion :: Union r w -> Union t w
    unsafeReUnion (Union v) = Union v
```

We need this for our last function, `decom`. This function partially
unfolds our `Union` into an `Either`

``` haskell
    decomp :: Typeable1 t => Union (t :> r) v -> Either (Union r v) (t v)
    decomp u = Right <$> prj u <?> Left (unsafeReUnion u)
```

This provides a way to actually do some sort of induction on `r` by
breaking out each type piece by piece with some absurd case for when
we don't have `a :> b`.

That about wraps up this little `Union` library, let's move on to see
how this is actually used.

## Control.Eff

Now let's talk about the core of extensible-effects,
`Control.Eff`. As always we'll start by taking a look at the export
list

``` haskell
    module Control.Eff(
                        Eff (..)
                      , VE (..)
                      , Member
                      , SetMember
                      , Union
                      , (:>)
                      , inj
                      , prj
                      , prjForce
                      , decomp
                      , send
                      , admin
                      , run
                      , interpose
                      , handleRelay
                      , unsafeReUnion
                      ) where
```

So right away we can see that we're exporting stuff `Data.Union1` as
well as several new things, including the infamous `Eff`.

The first definition we come across in this module is `VE`. `VE` is
either a simple value or a `Union` applied to a `VE`!

``` haskell
    data VE r w = Val w | E !(Union r (VE r w))
```

Right away we notice that "pure value or X" pattern we see with free
monads and other abstractions over effects.

We also include a quick function to try to extract a pure value form
`Val`s

``` haskell
    fromVal :: VE r w -> w
    fromVal (Val w) = w
    fromVal _ = error "extensible-effects: fromVal was called on a non-terminal effect."
```

Now we've finally reached the definition of `Eff`!

``` haskell
    newtype Eff r a = Eff { runEff :: forall w. (a -> VE r w) -> VE r w }
```

So `Eff` bears a striking resemblance to `Cont`. There are two
critical differences though, first is that we specialize our return
type to something constructed with `VE r`. The second crucial
difference is that by universally quantifying over `w` we sacrifice a
lot of the power of `Cont`, including `callCC`!

Next in `Control.Eff` is the instances for `Eff`

``` haskell
    instance Functor (Eff r) where
        fmap f m = Eff $ \k -> runEff m (k . f)
        {-# INLINE fmap #-}
    
    instance Applicative (Eff r) where
        pure = return
        (<*>) = ap
    
    instance Monad (Eff r) where
        return x = Eff $ \k -> k x
        {-# INLINE return #-}
    
        m >>= f = Eff $ \k -> runEff m (\v -> runEff (f v) k)
        {-# INLINE (>>=) #-}
```

Notice that these are all really identical to `Cont`s
instances. `Functor` adds a function to the head of the continuation.
`Monad` dereferences `m` and feeds the result into `f`. Exactly as
with `Cont`.

Next we can look at our primitive function for handling effects

``` haskell
    send :: (forall w. (a -> VE r w) -> Union r (VE r w)) -> Eff r a
    send f = Eff (E . f)
```

I must admit, this tripped me up for a while. Here's how I read it,
"provide a function, which when given a continuation for the rest of
the program expecting an `a`, produces a side effecting `VE r w` and
we'll map that into `Eff`".

Remember how `Union` holds functors? Well each of our effects must act
like as a functor and wrap itself in that union. By being open, we get
the "extensible" in extensible-effects.

Next we look at how to remove effects once they've been added to our
set of effects. In mtl-land, this is similar to the collection of
`runFooT` functions that are used to gradually strip a layer of
transformers away.

The first step towards this is to transform the CPS-ed effectful
computation `Eff`, into a more manageable form, `VE`

``` haskell
    admin :: Eff r w -> VE r w
    admin (Eff m) = m Val
```

This is a setup step so that we can traverse the "tree" of effects
that our `Eff` monad built up for us.

Next, we know that we can take an `Eff` with *no* effects and unwrap
it into a pure value. This is the "base case" for running an effectful
computation.

``` haskell
    run :: Eff () w -> w
    run = fromVal . admin
```

Concerned readers may notice that we're using a partial function, this
is OK since the `E` case is "morally impossible" since there is no `t`
so that `Member t ()` holds.

Next is the function to remove just one effect from an `Eff`

``` haskell
    handleRelay :: Typeable1 t
                => Union (t :> r) v -- ^ Request
                -> (v -> Eff r a)   -- ^ Relay the request
                -> (t v -> Eff r a) -- ^ Handle the request of type t
                -> Eff r a
    handleRelay u loop h = either passOn h $ decomp u
      where passOn u' = send (<$> u') >>= loop
```

Next to `send`, this function gave me the most trouble. The trick was
to realize that that `decomp` will leave us in two cases.

 1. Some effect producing a `v`, `Union r v`
 2. A `t` producing a `v`, `t v`

If we have a `t v`, then we're all set since we know exactly how to
map that to a `Eff r a` with `h`.

Otherwise we need to take this effect, add it back into our
computation. `send (<$> u')` takes the rest of the computation, that
continuation and feeds it the `v` that we know our effects
produce. This gives us the type `Eff r v`, where that outer
`Eff r` contains our most recent effect as well as everything
else. Now to convert this to a `Eff r a` we need to transform that `v`
to an `a`. The only way to do that is to use the supplied `loop`
function so we just bind to that.

Last but not least is a function to modify an effect
somewhere in our effectful computation. A `grep` reveals will see this
later with things like `local` from `Control.Eff.Reader` for example.

To do this we want something like `handleRelay` but without
removing `t` from `r`. We also need to generalize the type so that `t`
can be *anywhere* in our. Otherwise we'll have to prematurally
solidify our stack of effects to use something like `modify`.

``` haskell
    interpose :: (Typeable1 t, Functor t, Member t r)
              => Union r v
              -> (v -> Eff r a)
              -> (t v -> Eff r a)
              -> Eff r a
    interpose u loop h = maybe (send (<$> u) >>= loop) h $ prj u
```

Now this is almost identical to `handleRelay` except instead of using
`decomp` which will split off `t` and only works when `r ~ t :> r'`,
we use `prj`! This gives us a `Maybe` and since the type of `u`
doesn't need to change we just recycle that for the `send (<$> u) >>=
loop` sequence.

That wraps up the core of extensible-effects, and I must admit that
when writing this I was still quite confused as to actually *use*
`Eff` to implement new effects. Reading a few examples really helped
clear things up for me.

## Control.Eff.State

The `State` monad has always been the sort of classic monad example so
I suppose we'll start here.

``` haskell
    module Control.Eff.State.Lazy( State (..)
                                 , get
                                 , put
                                 , modify
                                 , runState
                                 , evalState
                                 , execState
                                 ) where
```

So we're *not* reusing the `State` from `Control.Monad.State` but
providing our own. It looks like

``` haskell
    data State s w = State (s -> s) (s -> w)
```

So what is this supposed to do? Well that `s -> w` looks a
continuation of sorts, it takes the state `s`, and produces the
resulting value. The `s -> s` looks like something that `modify`
should use.

Indeed this is the case

``` haskell
    modify :: (Typeable s, Member (State s) r) => (s -> s) -> Eff r ()
    modify f = send $ \k -> inj $ State f $ \_ -> k ()

    put :: (Typeable e, Member (State e) r) => e -> Eff r ()
    put = modify . const
```

we grab the continuation from `send` and add a `State` effect on top
which uses our modification function `s`. The continuation that
`State` takes ignores the value it's passed, the current state, and
instead  feeds the program computation the `()` it's expecting.

`get` is defined in a similar manner, but instead of modifying the
state, we use State's continuation to feed the program the current
state.

``` haskell
    get :: (Typeable e, Member (State e) r) => Eff r e
    get = send (inj . State id)
```

So we grab the continuation, feed it to a `State id` which won't
modify the state, and then inject that into our open union of effects.

Now that we have the API for working with states, let's look at how to
remove that effect.

``` haskell
    runState :: Typeable s
             => s                     -- ^ Initial state
             -> Eff (State s :> r) w  -- ^ Effect incorporating State
             -> Eff r (s, w)          -- ^ Effect containing final state and a return value
    runState s0 = loop s0 . admin where
     loop s (Val x) = return (s, x)
     loop s (E u)   = handleRelay u (loop s) $
                           \(State t k) -> let s' = t s
                                           in loop s' (k s')
```

`runState` first preps our effect to be pattern matched on with
`admin`. We then start `loop` with the initial state.

`loop` has two components, if we have run into a value, then we don't
interpret any effects, just stick the state and value together and
`return` them.

If we do have an effect, we use `handleRelay` to split out the `State s`
from our effects. To handle the case where we get a `VE w`, we just
`loop` with the current state. However, if we get a `State t k`, we
update the state with `t` and pass the continuation `k`.

From `runState` `evalState` and `execState`.

``` haskell
    evalState :: Typeable s => s -> Eff (State s :> r) w -> Eff r w
    evalState s = fmap snd . runState s
    
    execState :: Typeable s => s -> Eff (State s :> r) w -> Eff r s
    execState s = fmap fst . runState s
```

That wraps up the interface for `Control.Eff.State`. The nice bit is
this makes it a lot clearer how to use `send`, `handleRelay` and a few
other functions from the core.

## Control.Eff.Reader

Now we're on to `Reader`. The interesting thing here is that `local`
highlights how to use `interpose` properly.

As always, we start by looking at what exactly this module provides

``` haskell
    module Control.Eff.Reader.Lazy( Reader (..)
                                  , ask
                                  , local
                                  , reader
                                  , runReader
                                  ) where
```

The definition of `Reader` is refreshingly simple

``` haskell
    newtype Reader e v = Reader (e -> v)
```

Keen readers will note that this is just half of the `State`
definition which makes sense; `Reader` is half of `State`.

`ask` is defined almost identically to `get`

``` haskell
    ask :: (Typeable e, Member (Reader e) r) => Eff r e
    ask = send (inj . Reader)
```

We just feed the continuation for the program into `Reader`. A simple
wrapper over this gives our equivalent of `reads`

``` haskell
    reader :: (Typeable e, Member (Reader e) r) => (e -> a) -> Eff r a
    reader f = f <$> ask
```

Next up is `local`, which is the most interesting bit of this module.

``` haskell
    local :: (Typeable e, Member (Reader e) r)
          => (e -> e)
          -> Eff r a
          -> Eff r a
    local f m = do
      e <- f <$> ask
      let loop (Val x) = return x
          loop (E u) = interpose u loop (\(Reader k) -> loop (k e))
      loop (admin m)
```

So `local` starts by grabbing the view of the environment we're
interested in, `e`. From there we define our worker function which
looks a lot like `runState`. The key difference is that instead of
using `handleRelay` we use `interpose` to replace each `Reader` effect
with the appropriate environment. Remember that `interpose` is not
going to remove `Reader` from the set of effects, just update each
`Reader` effect in the current computation.

Finally, we simply rejigger the computation with `admin` and feed it
to `loop`.

In fact, this is very similar to how `runReader` works!

``` haskell
    runReader :: Typeable e => Eff (Reader e :> r) w -> e -> Eff r w
    runReader m e = loop (admin m)
      where
        loop (Val x) = return x
        loop (E u) = handleRelay u loop (\(Reader k) -> loop (k e))
```

## Control.Eff.Lift

Now between `Control.Eff.Reader` and `Control.Eff.State` I felt I had
a pretty good handle on most of what I'd read in
extensible-effects. There was just one remaining loose end:
`SetMember`. Don't remember what that was? It was a class in
`Data.OpenUnion1` that was conspicuously absent of detail or use.

I finally found where it seemed to be used! In `Control.Eff.Lift`.

First let's poke at the exports of his module

``` haskell
    module Control.Eff.Lift( Lift (..)
                           , lift
                           , runLift
                           ) where
```

This module is designed to lift an arbitrary monad into the world of
effects. There's a caveat though, since monads aren't necessarily
commutative, the order in which we run them in is very
important. Imagine for example the difference between `IO (m a)` and
`m (IO a)`.

So to ensure that `Eff` can support lifted monads we have to do some
evil things. First we must require that we never have to lifted
monads and we always run the monad last. This is a little icky but
it's usefulness outweighs such ugliness.

To ensure condition 1, we need `SetMember`.

``` haskell
    instance SetMember Lift (Lift m) (Lift m :> ())
```

So we define a new instance of `SetMember`. Basically this says that
any `Lift` is a `SetMember ... r` iff `Lift m` is the last item in
`r`.

To ensure condition number two we define `runLift` with the more
restrictive type

``` haskell
    runLift :: (Monad m, Typeable1 m) => Eff (Lift m :> ()) w -> m w
```

We can now look into exactly how `Lift` is defined.

``` haskell
    data Lift m v = forall a. Lift (m a) (a -> v)
```

So this `Lift` acts sort of like a "suspended bind". We postpone
actually binding the monad and simulate doing so with a continuation
`a -> v`.

We can define our one operation with `Lift`, `lift`.

``` haskell
    lift :: (Typeable1 m, SetMember Lift (Lift m) r) => m a -> Eff r a
    lift m = send (inj . Lift m)
```

This works by suspending the rest of the program in a our faux binding
to be unwrapped later in `runLift`.


``` haskell
    runLift :: (Monad m, Typeable1 m) => Eff (Lift m :> ()) w -> m w
    runLift m = loop (admin m) where
     loop (Val x) = return x
     loop (E u) = prjForce u $ \(Lift m' k) -> m' >>= loop . k
```

The one interesting difference between this function and the rest of
the run functions we've seen is that here we use `prjForce`. The
reason for this is that we know that `r` is just `Lift m :> ()`. This
drastically simplifies the process and means all we're essentially
doing is transforming each `Lift` into `>>=`.

That wraps up our tour of the module and with it, extensible-effects.

## Wrap Up

This post turned out a lot longer than I'd expected, but I think it
was worth it. We've gone through the coroutine/continuation based core
of extensible-effects and walked through a few different examples of
how to actually use them.

If you're still having some trouble putting the pieces together, the
rest of extensible effects is a great collection of useful examples of
building effects.

I hope you had as much fun as I did with this one!

*Thanks to Erik Rantapaa a much longer post than I led him to believe*

[ee-pack]: http://hackage.haskell.org/package/extensible-effects
[last-post]: /posts/2014-07-10-reading-logict.html
