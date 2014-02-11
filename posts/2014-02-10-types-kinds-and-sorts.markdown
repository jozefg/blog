---
title: Types and Kinds and Sorts, Oh My!
---

One subject that most introductory Haskell books fail to address is kinds.
This means that when most intermediate Haskellers start looking at Haskell
extensions they're flummoxed by `DataKinds`.

This posts aims to introduce people like this to what kinds and sorts are
and how they enter into the world of Haskell programming.

Think about an expression in Haskell, if it's well formed, then we can
assign it a type: `1 :: Int`, `"foo" :: String`, and `Just () :: Maybe ()`
for example.

Now this has all sorts of lovely benefits like ensuring we can't write insane
expressions like `"foo" + 2`. However, none of those benefits seem to extend
to the types themselves.

We have functions at the type level, consider

``` haskell
     type Cons a b = (a, b)
     cons a b = (a, b)
```

Clearly `Cons` is the type level equivalent of `cons`. But how
do we ensure that these actually work? What if we wrote

``` haskell
    foo :: Cons Maybe Either
    foo = ???
```

Clearly this makes no sense, there is no value whose type is `Maybe`.

This hints that we want something corresponding to a type system at the type level.
Something to ensure that all the types we write make some sort of sense. For example,
let's call the "type" of things values occupy, like `Int`, `String`, and `Maybe ()`, `*`.
So `Int :: *`. Now it seems that `Cons :: * -> * -> *`, or that `Cons` takes in two types,
`a` and `b`, and returns another type, `(a, b)`.

Now let's rattle off some other examples

``` haskell
    Maybe  :: * -> *
    Either :: * -> * -> *
    StateT :: (* -> *) -> * -> *
```

Notice that there's something interesting about `StateT`, it takes a *function* of
type onto type. It's a type level parallel of a higher order functions!

This is what *kinds* are all about, kinds are the type of types! Indeed, Haskell
even notates the kind of types that values occupy as `*` as well. We can read something like
`Int :: *` as *`Int` has the kind `*`*.  `StateT` is what's called a
higher *kinded* type, it's the type level version of higher order functions.

Now the obvious step is to ask, what's the "type" of a kind? `* :: ???` the answer
is, a *sort*. However, Haskell doesn't really talk much of sorts and only has one,
`BOX`. Because of this, we shall not speak much more of sorts.

Now back to kinds, what does Haskell have in terms of a kind system

 - By default, we have two kind constructors, `(->) :: BOX -> BOX -> BOX` and `* :: BOX`.
 - With `-XKindSignatures` we can actually utter kinds, eg `a :: *`.
 - With `-XDataKinds` we can define our own kinds just like we types.
 - With `-XTypeFamilies` we can write type level functions.
 - With `-PolyKinds` we have parametric polymophism at the kind level.

Breaking this down piece by pice

### Data Kinds
For the rest of the post I'll focus on showing how `DataKind`s meshes with Haskell's
kind system. The motiviation for `DataKind`s is that Haskell's vanilla kind system is
well... boring. It doesn't really help us since we can't write our own kinds and
types to occupy these kinds.

Let's take a simple example using GADTs, for the sake of bloat I'll leave
it up to the reader to go learn about GADTs if necessary.

``` haskell
    {-# LANGUAGE KindSignatures, GADTs, EmptyDataDecls #-}
    {- No DataKinds -}

    data Black -- This is what EmptyDataDecls allows,
    data Red   -- types with no constructors
    
    data Tree :: * -> * -> * where
      Leaf  :: Tree a Black
      NodeR :: a -> Tree a Black -> Tree a Black -> Tree a Red
      NodeB :: a -> Tree a c     -> Tree a c     -> Tree a Black
```

Here we're attempting to model the fact that in a red-black binary tree,
a red node has black children and a black node has either red or black children.

However this doesn't model it correctly, we'd like to make it impossible to state
nonsense like

``` haskell
    crazy :: Tree a Int
    crazy = undefined
```

The problem here is that the kind of `Tree` is `* -> * -> *`. Clearly we
don't really mean that a tree be colored by any type of kind `*`!

Enter `DataKinds`

``` haskell
    {-# LANGUAGE KindSignatures, DataKinds, GADTs #-}

    data Color = Red | Black
    
    data Tree :: * -> Color -> * where
      Leaf  :: Tree a Black
      NodeR :: a -> Tree a Black -> Tree a Black -> Tree a Red
      NodeB :: a -> Tree a c     -> Tree a c     -> Tree a Black
```

Now if we attempted

``` haskell
    foo :: Tree a Int
    foo = undefined
```

We'll get a kind error! This is a simple example of how we can leverage the kind system
to rule out illegal programs.

Let's attempt to encode a more complex property, that there are exactly the same number
of black nodes below every node. To start we'll need a type level encoding of numbers

``` haskell

```

These are called peano numbers, `Z` is zero and `S` is equivalent to `+1`, so 2 is `S (S Z)`.
Now we can integrate these into our tree.

``` haskell
    {-# LANGUAGE KindSignatures, DataKinds, GADTs #-}
    data Tree :: * -> Color -> Nat -> * where
      Leaf  :: Tree a Black Z
      NodeR :: a -> Tree a Black n -> Tree a Black n -> Tree a Red   n
      NodeB :: a -> Tree a c n     -> Tree a c n     -> Tree a Black (S n)
```

Taking a moment to examine this, we see that `Leaf`'s have a `Z` nodes below them,
which makes perfect sense. Then nodes take two trees of identical height and either
adds one to the height if the node is black or leaves it the same.

Now if we attempted to create an unbalanced tree

``` haskell
    unbalanced = NodeB () Leaf (NodeB () Leaf Leaf)
```

We get a type error! Hopefully this clears up what kinds
are and how we can leverage them to statically check some properties of
our programs.

For the curious reader I encourage you to look at `PolyKinds` and `TypeFamilies`,
these let you express some very sophisticated programs at the type level in Haskell.
If this really tickles your fancy, perhaps make the leap to Agda, Idris, or Coq and enjoy
full dependent types.
