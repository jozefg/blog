---
title: A Tutorial on Church Representations
---

I've written [a][cr1] [few][cr2] [times][cr3] about church
representations, but never aimed at someone at someone who'd never
heard of what a church representation is. In fact, it doesn't really
seem like too many people have!

In this post I'd like to fix that :)

## What is a Church Representation

Simply put, a church representation (CR) is a way of representing a piece
of concrete data with a function. The CR can be used through an
identical way to the concrete data, but it's comprised entirely of
functions.

They where originally described by Alanzo Church as a way of modeling
all data in lambda calculus, where all we have is functions.

## Tuples

The simplest CR I've found is that of a tuples.

Let's first look at our basic tuple API

``` haskell
    type Tuple a b = ...
    mkTuple :: a -> b -> Tuple a b
    fst     :: Tuple a b -> a
    snd     :: Tuple a b -> b
```

Now this is trivially implemented with `(,)`

``` haskell
    type Tuple a b = (a, b)
    mkTuple = (,)
    fst     = Prelude.fst
    snd     = Prelude.snd
```

The church representation preserves the interface, but changes all the
underlying implementations.

``` haskell
    type Tuple a b = forall c. (a -> b -> c) -> c
```

There's our church pair, notice that it's only comprised of `->`. It
also makes use of higher rank types. This means that a `Tuple a b` can
be applied to function producing *any* `c` and it must return
something of that type.

Let's look at how the rest of our API is implemented

``` haskell
    mkTuple a b = \f -> f a b
    fst tup     = tup (\a _ -> a)
    snd tup     = tup (\_ b -> b)
```

And that's it! 

It's helpful to step through some reductions here

``` haskell
    fst (mkTuple 1 2)
    fst (\f -> f 1 2)
    (\f -> f 1 2) (\a _ -> a)
    (\a _ -> a) 1 2
    1
```

And for `snd`

``` haskell
    snd (mkTuple True False)
    fst (\f -> f True False)
    (\f -> f True False) (\_ b -> b)
    (\_ b -> b) True false
    False
```

So we can see that these are clearly morally equivalent. The only real
question here is whether, for each CR tuple there exists a normal
tuple. This isn't immediately apparent since the function type for the
CR looks a lot more general. In fact, the key to this proof lies in
the `forall c` part, this extra polymorphism let's us use a powerful
technique called "parametricity" to prove that they're equivalent.

I won't actually go into such a proof now since it's not entirely
relevant, but it's worth noting that both `(,)` and `Tuple` are
completely isomorphic.

To convert between them is pretty straightforward

``` haskell
    isoL :: Tuple a b -> (a, b)
    isoL tup = tup (,)

    isoR :: (a, b) -> Tuple a b
    isoR (a, b) = \f -> f a b
```

Now that we have an idea of how to church representations "work" let's
go through a few more examples to start to see a pattern.

## Booleans

Booleans have the simplest API of all

``` haskell
    type Boolean = ...
    true  :: Boolean
    false :: Boolean
    test  :: Boolean -> a -> a -> a
```

We can build all other boolean operations on `test`

``` haskell
    a && b = test a b false
    a || b = test true b
    when t e = test t e (return ())
```

This API is quite simple to implement with `Bool`,

``` haskell
    type Boolean = Bool

    true  = True
    false = False
    test b t e = if b then t else e
```

But how could we represent this with functions? The answer stems from
`test`,

``` haskell
    type Boolean = forall a. a -> a -> a
```

Clever readers will notice this is almost identical to `test`, a
boolean get's two arguments and returns one or the other.

``` haskell
    true  = \a _ -> a
    false = \_ b -> b
    test b t e = b t e
```

We can write an isomorphism between `Bool` and `Boolean` as well

``` haskell
    isoL :: Bool -> Boolean
    isoL b = if b then true else false

    isoR :: Boolean -> Bool
    isoR b = test b True False
```

## Lists

Now let's talk about lists. One of the interesting things is lists are
the first recursive data type we've dealt with so far.

Defining the API for lists isn't entirely clear either. We want a
small set of functions that can easily cover *any* conceivable
operations for a list.

The simplest way to do this is to realize that we can do exactly 3
things with lists.

 1. Make an empty list
 2. Add a new element to the front of an existing list
 3. Pattern match on them

We can represent this with 3 functions

``` haskell
    type List a = ...

    nil   :: List a
    cons  :: a -> List a -> List a
    match :: List a -> b -> (a -> List a -> b) -> b
```

If `match` looks confusing just remember that

``` haskell
    f = match list g h
```

Is really the same as

``` haskell
    f []       = g
    f (x : xs) = h x xs
```

In this way `match` is just the pure functional version of pattern
matching. We can actually simplify the API by realizing that rather
than this awkward `match` construct, we can use something cleaner.

`foldr` forms a much more pleasant API to work with since it's really
the most primitive form of "recursing" on a list.

``` haskell
    match :: List a -> (a -> List a -> b) -> b -> b
    match list f b = fst $ foldr list worker (b, nil)
      where worker x (b, xs) = (f x xs, cons x xs)
```

The especially nice thing about `foldr` is that it doesn't mention
`List a` in it's too "destruction" functions, all the recursion is
handled in the implementation.

We can implement CR lists trivially using `foldr`

``` haskell
    type List a = forall b. (a -> b -> b) -> b -> b

    nil = \ _ nil -> nil
    cons x xs = \ cons nil -> x `cons` xs cons nil
    foldr list cons nil = list cons nil
```

Notice that we handle the recursion in the list type by having a `b`
as an argument? This is similar to how the accumulator to `foldr`
gets the processed tail of the list. This is a common technique for
handling recursion in our church representations.

Last but not least, the isomorphism arises from `foldr (:) []`,

``` haskell
    isoL :: List a -> [a]
    isoL l = l (:) []

    isoR :: [a] -> List a
    isoR l f z = foldr f z l

## Either

The last case that we'll look at is `Either`. Like `Pair`, `Either`
has 3 different operations.

``` haskell
    type Or a b = ...
    inl :: a -> Or a b
    inr :: b -> Or a b
    
    or :: Or a b -> (a -> c) -> (b -> c)  -> c
````

This is pretty easy to implement with `Either`

``` haskell
    type Or a b = Either a b
    inl = Left
    inr = Right
    
    or (Left a)  f g = f a
    or (Right b) f g = g b
```

Once again, the trick to encoding this as a function falls right out
of the API. In this case we use the type of `or`

``` haskell
     type Or a b = forall c. (a -> c) -> (b -> c) -> c
  
    inl a = \f g -> f a
    inr b = \f g -> g a

    or x = x
```

Last but not least, let's quickly rattle off our isomorphism.

``` haskell
    isoL :: Or a b -> Either a b
    isoL o = o Left Right

    isoR o :: Either a b -> Or a b
    isoR o = or o
```

## The Pattern

So now we can talk about the underlying pattern in CRs. First remember
that for any type `T`, we have a list of `n` distinct constructors `T1`
`T2` `T3`...`Tn`. Each of the constructors has a `m` fields `T11`,
`T12`, `T13`...

Now the church representation of such a type `T` is

``` haskell
    forall c.  (T11 -> T12 -> T13 -> .. -> c)
            -> (T21 -> T22 -> T23 -> .. -> c)
            ...
            -> (Tn1 -> Tn2 -> Tn3 -> .. -> c)
            -> c
```

This pattern doesn't map quite as nicely to recursive types. Here we
have to take the extra step of substituting all occurrences of `T` for
`c` in our resulting church representation.


This is actually such a pleasant pattern to work with that I've
written a [library][gchurch] for automatically reifying a type between its
church representation and concrete form.

## Wrap Up

Hopefully you now understand *what* a church representation is. It's
worth noting that a lot of stuff Haskellers stumble upon daily are
really church representations in disguise.

My favorite example is `maybe`, this function takes a success and
failure continuation with a `Maybe` and produces a value. With a
little bit of imagination, one can realize that this is really just 
a function mapping a `Maybe` to a church representation!

If you're thinking that CRs are pretty cool! Now might be a time to
take a look at one of my previous [posts][cr1] on deriving them
automagically.

[cr1]: /posts/2014-03-06-church.html
[cr2]: /posts/2014-03-07-church-the-sequel.html
[cr3]: /posts/2014-03-10-revenge-of-churchrep.html
[gchurch]: http://hackage.haskell.org/package/generic-church

