---
title: You Could Have Invented GHC.Generics
---

In Haskell right now there seem to be two main approaches to data-generic
programming. There's the whole `Typeable`/`Data` approaches which is a bit
magic. Lately however, there's been a new kid on the block, GHC.Generics.

In this post we'll step through the intuition for the library and (hopefully)
help shed some light on why it exists and how to use it.

## Boilerplate
Let's imagine you, our young and brilliant Haskell hacker cranking out some
code. You've probably gone the typesafe route and have lots and lots of types
to encode invariants.

However, this proliferation of types is cramping your style a bit, you're forced
to create a new function over each type which seems to do exactly the same thing!

``` haskell
    mapFoo  :: (a -> b) -> Foo a  -> Foo b
    mapBar  :: (a -> b) -> Bar a  -> Bar b
    mapQuux :: (a -> b) -> Quux a -> Quux b
```

But, we're clever enough to notice that this is obviously just `fmap`!
So we can scrap all of this with `fmap` and `-XDeriveFunctor`.

But, what about other functions. There are a lot of things that are
basically mechanical to define over each type. Serialization, field
selection, and so on and so on. Each of these operations have something
in common; they deal with the *structure* of the types rather
than the actual representation of it.

Selecting the first fields from

``` haskell
    data Foo a = Foo a a a
    data Bar a = Bar a a a
```

is almost identical! The only difference is in the name. So,
let's figure out a way to talk about the structure of our types.

## Dissecting an Algebraic Type
Now, when we go to dissect some type `data Foo = ...` we have two things
to consider

 1. A list of constructors
 2. A list of fields for each constructor

Let's start with (2) since it's simpler. For types that are of the form

``` haskell
    data SomeType = OneConstructor field1 field2 field3 ...
```

we can almost think of them as really, really big tuples.

``` haskell
    type SomeType' = (field1, field2, field3, ...)
```

But, since we want to encode different numbers of fields in
just one type, let's transform this further into

``` haskell
    type SomeType'' = (field1, (field2, (field3, ...)))
```

There we have it, we can encode lists of fields as
a deeply nested group of tuples.

We can now imagine something like

``` haskell
    {-# LANGUAGE TypeFamilies #-}
    type family TupleForm a

    data Foo a = Foo a a
    type instance TupleForm (Foo a) = (a, a)

    data Bar a = Bar a a
    type instance TupleForm (Bar a) = (a, a)

    class Tuple a where
       toT :: a -> TupleForm a
       fromT :: TupleForm a -> a

    instance Tuple (Foo a) where
     ...
    instance Tuple (Bar a) where
     ...

```

Now we can write generic functions by only writing them for the `TupleForm` of `Foo` and `Bar`.
For example,

``` haskell
    gfst :: (TupleForm a ~ (b, c), Tuple a) => a -> b
    gfst = fst . toF 
```

Now that we understand fields, let's move on to constructors!

A list constructors is the dual to a list of fields, representing
OR rather than AND. We can make a bit of a leap from this to thinking
that our representations of the two should be dual. So what would be the
dual of `(a, b)`? Why that would be `Either a b`!

This means for a type

``` haskell
    data SomeType = Bar Int | Baz Char | Quux ()
    type SomeType' = Either Int (Either Char ())
```

This covers almost every case, we just need to make
sure we represent no argument constructors as constructors
of one argument: `()`. Take a moment to think why.

## A Procedure for Reifying
Let's now outline an algorithm for turning
some arbitrary type to the corresponding
generic version.

For a type C, with constructors C1, C2, C3.. and
fields C1^1, C1^2, C2^1...

 1. Change each set Cx^* to the `TupleForm`, call this `TupleForm` Tx
 2. Nest the `Tx`'s in `Either`'s, `Either T1 (Either T2 (Either T3 ...))`

And that's it, let's practice on some data types to check that it works.

``` haskell
    data Test = Foo Int Char | Bar Int Bool Char | Quux
    type Test' = Either (Int, Char) (Either (Int, (Bool, Char)) ())

    data Maybe a  = Just a | Nothing
    type Maybe' a = Either a ()
```

So we can see that this transformation is pretty mechanical!
There's one hiccup though: what do we do with recursive types?

We'll handle it the same way that GHC.Generics does, we just don't
transform the recursive arguments into the generic representation
lest we end up with an infinite tree.

So `[a]` should look like

``` haskell
    type List a = Either (a, [a]) ()
```

## Building a Library
Now if we want to build this into a library, we'd
like to provide a few of our own data types rather
than hijacking `Either` and `(,)`.

``` haskell
    {-# LANGUAGE TypeOperators #-}

    data (:*:) a b = a :*: b       -- Like (,) a b
    data (:+:) a b = InL a | InR b -- Like Either a b
    data U         = U             -- Like (), U is for Unit
```

Now all our transformation are the same, but the results
are prettier thanks to the type level operators

``` haskell
    type List   a = (a :*: [a]) :+: U
    type Maybe' a = a :+: U
```

Now to facilitate generic programming, we'll lug one more parameter
through each of these constructors and add another two types to wrap
meta information and constants respectively

``` haskell
    data (:*:) a b p = a p :*: b p           -- Like (,) a b
    data (:+:) a b p = L1 (a p) | R1 (b p) -- Like Either a b
    data U         p = U                     -- Like (), U is for Unit

    newtype M1 i c f p = M1 (f p) -- i and c are meta info
    newtype K1 i c   p = K1 c
```

Now because we're expecting all our arguments to `(:*:)` and
`(:+:)` to be of kind `* -> *` we use `K1` to wrap a normal type
like `Int` so that it can take an argument.

`M1` is a bit odd, it's used to store information about our data
entirely in phantom types. We can imagine having a bunch of types that represent different things,
like whether the tree of constructors represents such and such data type
or what constructor we're dealing with. It's not terribly relevant to the
rest of this post, but useful in some odd cases.

Now we can repeat our transformation we'd discussed earlier just using the new
constructors instead. We can imagine wrapping up this whole class like this

``` haskell
    class Generic a where
      type family Rep a :: * -> *
      to   :: a       -> Rep a
      from :: Rep a p -> a
```

This is very much in the spirit of our `Tuple` type class,
but now our type family returns something of type `* -> *` to leave
room for our extra `p` parameter

## The Real Deal
As clever readers will have noticed, the above type class is
precisely what `GHC.Generics` exports! We have successfully reached
full circle and now have arrived at `GHC.Generics'` API.

The only difference between us and GHC.Generics is their `Generic`
class can be derived almost identically to our algorithm. The only
slight difference is rather than a "list" of `:*:`'s or `:+:`'s they
make a tree, this makes little difference to most programs however.

To wrap things up, let's finish by showcasing making a simple
generic debugging dumper.

To begin with, we'll define a class `GDump` and will make instances
for the GHC.Generics types

``` haskell
    class GDump a where
       gdump :: a -> String

    instance GDump (U1 p) where
      gdump U1 = "()"

    instance Show c => GDump (K1 i c p) where
      gdump (K1 c) = show c

    instance (GDump (f p), GDump (g p)) => GDump ((:*:) f g p) where
      gdump (a :*: b) = "(" ++ gdump a ++ " :*: " ++ gdump b ++ ")"

    instance (GDump (f p), GDump (g p)) => GDump ((:+:) f g p) where
      gdump (L1 a) = "(Left  " ++ gdump a ++ ")"
      gdump (R1 a) = "(Right " ++ gdump a ++ ")"

    instance (GDump (f p)) => GDump (M1 a b f p) where
      gdump (M1 f) = gdump f
```

And now we can create a class for "normal" values and use
`-XDefaultSignatures` to give the default implementation
a `Generic` constraint

``` haskell
    class Dump a where
      dump :: a -> String

      default dump :: (Generic a, GDump (Rep a ())) => a -> String
      dump a = gdump (from' a)
        where from' :: Generic a => a -> Rep a ()
              from' = from  -- A hack to stop the type checker from whining about p
```

And now we can just use this default implementation.

``` haskell
    instance Show a => Dump (Maybe a)
```

Using this we can print suitably boring representations of
generic types, for free!
