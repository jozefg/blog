---
title: Types and Kinds and Sorts, Oh My!
---

One subject that most introductory Haskell books fail to address is kinds.
This means that when most intermediate Haskellers start looking at Haskell
extensions they're flummoxed by `DataKinds`.

This post aims to introduce intermediate haskellers to kinds and sorts
as well as how they enter into the world of Haskell programming.

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

`Cons` looks like the type level equivalent of `cons`. But how
do we ensure that these actually work? What if we wrote

``` haskell
    foo :: Cons Maybe Either
    foo = ???
```

This makes no sense however, there is no value whose type is `Maybe`.

This hints that we want something corresponding to a type system at the type level.
Something to ensure that all the types we write make some sort of sense. For example,
let's call the "type" of types things values occupy, `*`. So `Int :: *`,
as well as `String`, `[a]`, and many others. Now it seems that `Cons` takes in two types,
`a :: *` and `b :: *`, and returns another type, `(a, b) :: *`. This is notated `* -> * -> *`.

Now let's rattle off some other examples

``` haskell
    Maybe  :: * -> *      -- Maybe takes a type and returns another
    Either :: * -> * -> * -- Either takes two types and returns another
    StateT :: (* -> *) -> * -> *
```

Notice that there's something interesting about `StateT`, it takes a *function* of
type onto type. It's a type level parallel of a higher order function!

This is what *kinds* are all about, kinds are the type of types! Indeed, Haskell
even notates the kind of types that values occupy as `*` as well. We can read something like
`Int :: *` as *`Int` has the kind `*`*.

`StateT` is what's called a higher *kinded* type, it's the type level
version of higher order functions.

Now the step is to ask, what's the "type" of a kind? `* :: ???` the answer
is, a *sort*. However, Haskell doesn't talk  of sorts and conceptually only has one,
`BOX`. It is occasionally helpful to think as if `BOX` existed,
but we can't actually state this in Haskell. This means that while the sorts
*conceptually exist*, we can't really do much of anything with them.
Perhaps in the future Haskell will grow a few more extensions to enable
talking about sorts, until then though, we'll focus on kinds.

Now back to kinds, what does Haskell have in terms of a kind system

 - By default, we have two kind constructors, `(->) :: BOX -> BOX -> BOX` and `* :: BOX`.
 - With `-XKindSignatures` we can actually utter kinds, eg `a :: *`.
 - With `-XDataKinds` we can define our own kinds just like we can with types.
 - With `-XTypeFamilies` we can write type level functions.
 - With `-XPolyKinds` we have parametric polymorphism at the kind level.

### Data Kinds
The motiviation for `DataKind`s is that Haskell's vanilla kind system is
well... boring. It doesn't really help us since we can't write our own kinds and
types to occupy these kinds.

Let's take a simple example using GADTs and red-black binary trees. For the sake of brevity
I'll leave the reader to take a moment and [learn](http://www.haskell.org/haskellwiki/GADTs_for_dummies)
about GADTs if necessary.

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

The problem here is that the kind of `Tree` is `* -> * -> *`. We
don't really mean that a tree be colored by any type of kind `*`!
We really want to limit it so that we can only color a tree with `Red`
and `Black`.


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
data Nat = Z | S Nat
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

Taking a moment to examine this, we see that a `Leaf` has 0 black nodes below it,
which makes perfect sense. Then nodes take two trees of identical height and either
adds one to the height if the node is black or leaves it the same.

Now if we attempted to create an unbalanced tree

``` haskell
    unbalanced = NodeB () Leaf (NodeB () Leaf Leaf)
```

We get a type error!

Hopefully this clears up what kinds are and how we can leverage them to
statically check some properties of our programs.

For the curious reader I encourage you to look at `PolyKinds` and `TypeFamilies`,
these let you express some very sophisticated programs at the type level in Haskell.
If this really tickles your fancy, perhaps make the leap to Agda, Idris, or Coq to enjoy
full dependent types.

*Thanks to GlenH7 and JimmyHoffa on thewhiteboard and byorgey on #haskell for proof reading*
