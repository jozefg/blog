---
title: Church Representations: Part 2
---

In the [last](/posts/2014-03-06-church.html) post, we discussed some of the
type families needed to transform a type equipped with a `Generic` instance
into a church representation.

In this post, we'll go over the type class prolog needed to actually
mechanically transform values between these types.

To start with, we'll need a bit of boilerplate. Here's our language extensions
and imports.

``` haskell
    {-# LANGUAGE TypeFamilies,          TypeOperators,     UndecidableInstances #-}
    {-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts     #-}
    {-# LANGUAGE ScopedTypeVariables,   RankNTypes                              #-}
    import GHC.Generics
    import Data.Proxy
```

Now our first order of business is to write a function to take a `GHC.Generic`s value
and transform it into the corresponding value occupying the type returned by `StripMeta`.

To do this we rely on type classes

``` haskell
    class GStripMeta a where
      stripMeta :: a -> StripMeta a
    instance GStripMeta (f p) => GStripMeta (M1 a b f p) where
      stripMeta (M1 f) = stripMeta f
    instance GStripMeta (K1 a t p) where
      stripMeta = id
    instance GStripMeta (U1 p) where
      stripMeta = id
    instance (GStripMeta (l p), GStripMeta (r p),
              (WithoutParam (StripMeta (l p))) p ~ StripMeta (l p),
              (WithoutParam (StripMeta (r p))) p ~ StripMeta (r p)) =>
             GStripMeta ((:*:) l r p) where
      stripMeta (l :*: r) = stripMeta l :*: stripMeta r
    instance (GStripMeta (l p), GStripMeta (r p),
              (WithoutParam (StripMeta (l p))) p ~ StripMeta (l p),
              (WithoutParam (StripMeta (r p))) p ~ StripMeta (r p)) =>
             GStripMeta ((:+:) l r p) where
      stripMeta (L1 l) = L1 $ stripMeta l
      stripMeta (R1 r) = R1 $ stripMeta r
```

This does some type class prolog to traverse a GHC.Generics value and systematically
throw out the `M1` annotations. An important technique that I make heavy use of is
the `~` equality constraints. These let us assert that two types are equivalent
and the type checker will attempt to verify this once it's selected the appropriate
instance later. Other than that there's not too much of interest here so let's move
on to something with more substance.

Our `ToList` type family requires quite a bit of work to automagically do with a type
family. The base cases are pretty straightforward

``` haskell
    class GList a r where
      toList :: Maybe a -> r -> ToList a r
    instance (WithoutParam r) p ~ r => GList (U1 p) r where
      toList Nothing  r = R1 r
      toList (Just a) _ = L1 a
    instance (WithoutParam r) p ~ r => GList (K1 a t p) r where
      toList Nothing  r = R1 r
      toList (Just a) _ = L1 a
    instance (WithoutParam r) p ~ r => GList ((l :*: r') p) r where
      toList Nothing  r = R1 r
      toList (Just a) _ = L1 a
```
The trick here is that if we get a `Nothing`, it means that somewhere in
the process of choosing the list we've already found state our sum type is
in and all we do is pass things of to `R1`. Otherwise, we shove the value
we receive into `L1`.

Now the tricky bit is the `:+:` instance which must walk along the spine of
our tree flattening things as it goes.


``` haskell
    instance (GList (l p) (ToList (r' p) r), GList (r' p) r) =>
             GList ((l :+: r') p) r where
      toList (Just sum@(L1 l)) r = toList (Just l) (toList (rNot sum) r)
        where rNot :: forall l r p. (l :+: r) p -> Maybe (r p)
              rNot _ = Nothing
      toList (Just sum@(R1 r')) r = toList (lNot sum) (toList (Just r') r)
        where lNot :: forall l r p. (l :+: r) p -> Maybe (l p)
              lNot _ = Nothing
      toList m r = toList (lNot m) (toList (rNot m) r)
        where lNot :: forall l r p. Maybe ((:+:) l r p) -> Maybe (l p)
              lNot _ = Nothing
              rNot :: forall l r p. Maybe ((:+:) l r p) -> Maybe (r p)
              rNot _ = Nothing
```

We're just plugging in the appropriate `l` and `r` into `toList l (toList r rest)`.
if both we have `Just (L1 l)` then we put in `Nothing` for `r`, similarly for `Just (R1 r)`
and if we have `Nothing` then both are filled in as `Nothing`s.

Notice that we have to a few hoops using `lNot` and `rNot`. Otherwise GHC will complain
that type classes aren't injective and it's not sure how to handle `Nothing :: Maybe a`.
However, a bit of explicit hand holding takes care of this.

For our next type class hacking we need to actually add one type family
that I forgot in our last post.

``` haskell
    type family ToListProd v rest
    type instance ToListProd ((:*:) l r' p) r = ToListProd (l p) (ToListProd (r' p) r)
    type instance ToListProd (K1 a t p)     r = (K1 a t     :*: WithoutParam r) p
    type instance ToListProd (U1 p)         r = (U1         :*: WithoutParam r) p
```

This is isomorphic to `ToList` but instead of restructuring `:+:`'s, it moves around
`:*:`'s. The corresponding type class for this almost identical to `GList`

``` haskell
    class GListProd a r where
      toListProd :: a -> r -> ToListProd a r
    instance (WithoutParam r) p ~ r => GListProd (U1 p) r where
      toListProd = (:*:)
    instance (WithoutParam r) p ~ r => GListProd (K1 a t p) r where
      toListProd = (:*:)
    instance (GListProd (l p) (ToListProd (r' p) r), GListProd (r' p) r) =>
             GListProd ((:*:) l r' p) r where
      toListProd (l :*: r) rest = toListProd l (toListProd r rest)
```

The only notable difference here is that we don't have a `Maybe a` since with products both
sides our present. This makes the whole thing much simpler.

No we're ready to proceed to the actual transformation type classes.

``` haskell
    class GChurchProd a where
      prod :: Proxy r -> a -> ChurchProd a r -> r -- Proxy needed for GChurchSum
    instance GChurchProd (U1 p) where
      prod _ _ f = f
    instance GChurchProd (K1 a t p) where
      prod _ (K1 r) f = f r
    instance GChurchProd (r p) => GChurchProd ((:*:) (K1 a t) r p) where
      prod p (K1 l :*: r) f = prod p r (f l)
    
    class Swallow a where
      swallow :: Proxy a -> c -> ChurchSum a c
    instance Swallow (ListTerm p) where
      swallow _ c = c
    instance Swallow (r p) => Swallow ((:+:) l r p) where
      swallow p c = \_ -> swallow (right p) c
        where right :: forall l r p. Proxy ((:+:) l r p) -> Proxy (r p)
              right _ = Proxy
```
In `GChurchProd` we take a value and the corresponding product eliminator
and eliminate it. Believe it or not this is essentially the workhorse of this
entire library. We're threading a `Proxy r` through there which will come
in handy when we start to use this in `GChurchSum`.

`Swallow` is a bit odd. It represents a situation where we've already used
`prod` to produce our result and now need to eat the rest of the supplied
arguments. Note again the use of `Proxy` to keep track of types, it's a useful
little library!

Now, finally, `GChurchSum`

``` haskell
    class GChurchSum a r where
      elim :: Proxy r -> a -> ChurchSum a r -- Proxy because type inference is stubborn
    
    instance (GListProd (l p) (ListTerm ()), GChurchProd (ToListProd (l p) (ListTerm ())),
              GChurchSum (r' p) r, Swallow (r' p)) =>
             GChurchSum ((:+:) l r' p) r where
      elim p sum@(L1 l) = \f ->
        swallow (right sum) (prod p (toListProd l (ListTerm :: ListTerm ())) f)
        where right :: forall l r p. (:+:) l r p -> Proxy (r p)
              right _ = Proxy
      elim p (R1 r) = \_ -> elim p r
    instance GChurchSum (ListTerm p) r where
      elim _ _ = error "Malformed generic instance"
```

Now the `elim` instance for `ListTerm` can never be called, this is
guarenteed by the definition of `toList` since a type must occupy one
state prior to `ToList`, we'll never end up with `ListTerm`. 

Otherwise if we get an `L1` then we're at the actual value
our sum type is in so we produce an `r` and swallow the rest
of our arguments, notice that this is where we actual
transform a leaf into a `ToListProd` and this is reflected in
our constraints. Otherwise we ignore the irrelevant eliminator
and recurse!

To put it all together

``` haskell
    from' :: Generic a => a -> Rep a ()
    from' = from
    
    toChurch :: forall a r. 
                (Generic a, GStripMeta (Rep a ()),
                 GList (StripMeta (Rep a ())) (ListTerm ()),
                 GChurchSum (ToList (StripMeta (Rep a ())) (ListTerm ())) r) =>
                a -> Church a r
    toChurch = elim p . flip toList (ListTerm :: ListTerm ()) . Just . stripMeta . from'
      where p = Proxy :: Proxy r
```

And we're done! Using this we can reify a type to it's church representation. We
can mostly ignore that scary looking constraints on `toChurch` since they *should*
be true by construction.

As a demo

    > toChurch [1, 2, 3] True (\_ _ ->  False)
    False
    > toChurch [] True (\_ _ ->  False)
    True

The `True` corresponds to the `[]` list case and the function
represents `(:)`. The current `True (\_ _ -> False)` actually
computes `null`.

*Edit, added GListProd*
