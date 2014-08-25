---
title: Introduction to Dependent Types: Haskell on Steroids
---

I'd like to start another series of blog posts. This time on something
that I've wanted to write about for a while, dependent types.

There's a noticeable lack of accessible materials introducing
dependent types at a high level aimed at functional
programmers. That's what this series sets out help fill. Therefore, if
you're a Haskell programmer and don't understand something, it's a
bug! Please comment so I can help make this a more useful resource for
you :)

There are four parts to this series, each answering one question

 1. What are dependent types?
 2. What does a dependently typed language look like?
 3. What does it feel like to write programs with dependent types?
 4. What does it mean to "prove" something

So first things first, what are dependent types? Most people by now
have heard the unhelpful quick answer

> A dependent type is a type that depends on a value, not just other
> types.

But that's not helpful! What does this actually look like? To try to
understand this we're going to write some Haskell code that pushes us
as close as we can get to dependent types in Haskell.

## Kicking GHC in the Teeth

Let's start with the flurry of extensions we need

    {-# LANGUAGE DataKinds            #-}
    {-# LANGUAGE KindSignatures       #-}
    {-# LANGUAGE GADTs                #-}
    {-# LANGUAGE TypeFamilies         #-}
    {-# LANGUAGE UndecidableInstances #-}

Now our first definition is a standard formulation of natural numbers

``` haskell
    data Nat = Z | S Nat
```

Here `Z` represents 0 and `S` means `+ 1`. So you should read `S Z` as
1, `S (S Z)` as 2 and so on and so on.

If you're having some trouble, this function to convert an `Int` to a
`Nat` might help

``` haskell
    -- Naively assume n >= 0
    toNat :: Int -> Nat
    toNat 0 = Z
    toNat n = S (toNat $ n - 1)
```

We can use this definition to formulate addition

``` haskell
    plus :: Nat -> Nat -> Nat
    plus Z n     = n
    plus (S n) m = S (plus n m)
```

This definition proceeds by "structural induction". That's a scary
word that pops up around dependent types. It's not all that
complicated, all that it means is that we use recursion only on
*strictly smaller terms*.

There is a way to formally define smaller, if a term is a constructor
applied to several (recursive) arguments. Any argument to the
constructor is strictly smaller than the original terms. In a strict
language if we restrict ourselves to only structural recursion we're
guaranteed that our function will terminate. This isn't quite the
case in Haskell since we have infinite structures.

``` haskell
    toInt :: Nat -> Int
    toInt (S n) = 1 + toInt n
    toInt Z     = 0

    bigNumber = S bigNumber

    main = print (toInt bigNumber) -- Uh oh!
```

Often people will cheerfully ignore this part of Haskell when talking
about reasoning with Haskell and I'll stick to that tradition (for now).

Now back to the matter at hand. Since our definition of `Nat` is quite
straightforward, it get's promoted to the [kind][kind-exp] level by
`DataKinds`.

Now we can "reflect" values back up to this new kind with a second
GADTed definition of natural numbers.

``` haskell
    data RNat :: Nat -> * where
      RZ :: RNat Z
      RS :: RNat n -> RNat (S n)
```

Now, let's precisely specify the somewhat handwavy term
"reflection". I'm using it in the imprecise sense meaning that we've
lifted a value into something isomorphic at the type level. Later
we'll talk about reflection precisely mean lifting a value into the
type level. That's currently not possible since we can't have values
in our types!

What on earth could that be useful for? Well with this we can
do something fancy with the definition of addition.


``` haskell
    type family Plus n m :: Nat where
      Plus Z n     = n
      Plus (S n) m = S (Plus n m)
```

Now we've reflected our definition of addition to the type
family. More than that, what we've written above is fairly obviously
correct. We can now force our value level definition of addition to
respect this type family

``` haskell
    plus' :: RNat n -> RNat m -> RNat (Plus n m)
    plus' RZ n     = n
    plus' (RS n) m = RS (plus' n m)
```

Now if we messed up this definition we'd get a type error!

``` haskell
    plus' :: RNat n -> RNat m -> RNat (Plus n m)
    plus' RZ n     = n
    plus' (RS n) m = plus' n m -- Unification error! n ~ S n
```

Super! We know have types that express strict guarantees about our
program. But how useable is this?

To put it to the test, let's try to write some code that reads to
integers for standard input and prints their sum.

We can easily do this with our normal `plus`

``` haskell
    readNat :: IO Nat
    readNat = toNat <$> readLn

    main :: IO ()
    main = plus <$> readNat <*> readNat
```

Easy as pie! But what about `RNat`, how can we convert a `Nat` to an
`RNat`? Well we could try something with type classes I guess

    class Reify a where
      type N
      reify :: a -> RNat N


But wait, that doesn't work since we can only have once instance for
all `Nat`s. What if we did the opposite

    class Reify (n :: Nat) where
      nat :: RNat n -> Nat

This let's us go in the other direction.. but that doesn't help us! In
fact there's no obvious way to propagate runtime values back into the
types. We're stuck.

## GHC with Iron Dentures

Now, if we could add some magical extension to GHC could we write
something like above program? Yes of course! The key idea is to not
reflect up our types with data kinds, but rather just allow the values
to exist in the types on their own.

For these I propose two basic ideas

 1. A special reflective function type
 2. Lifting expressions into types

For our special function types, we allow the return *type* to use the
supplied *value*. These are called pi types. We'll give this the
following syntax

    (x :: A) -> B x

Where `A :: *` and `B :: Nat -> *` are some sort of type. Notice that
that `Nat` in `B`'s kind isn't the data kind promoted version, but
just the goodness to honest normal value.

Now in order to allow `B` to actually make use of it's supplied value,
our second idea let's normal types be indexed on values! Just like how
GADTs can be indexed on types. We'll call these GGADTs.

So let's define a new version of `RNat`

``` haskell
    data RNat :: Nat -> * where
      RZ : RNat Z
      RS : RNat n -> RNat (S n)
```

This looks exactly like what we had before, but our semantics
are different now. Those `Z`'s and `S`'s are meant to represent actual
values, not members of some kind. There's no promoting types to
singleton kinds anymore, just plain old values being held in fancier types.

Because we can depend on normal values, we don't even have to use our
simple custom natural numbers.

``` haskell
    data RInt :: Int -> * where
      RZ :: RInt 0
      RS :: RInt n -> RInt (1 + n)
```

Notice that we allowed our types to call functions, like `+`. This can
potentially be undecidable, something that we'll address later.

Now we can write our function with a combination of these two ideas

``` haskell
    toRInt :: (n :: Int) -> RInt n
    toRInt 0 = RZ
    toRInt n = RS (toRInt $ n - 1)
```

Notice how we used pi types to change the return type dependent on the
input *value*. Now we can feed this any old value, including ones we
read from standard input.


``` haskell
    main = print . toInt $ plus' <$> fmap toRInt readLn <*> fmap toRInt readLn
```

Now, one might wonder how the typechecker could possibly know how to
handle such things, after all how could it know what'll be read from
stdin!

The answer is that it doesn't. When a value is reflected to the type
level we can't do anything with it. For example, if we had a type like

``` haskell
    (n :: Int) -> (if n == 0 then Bool else ())
```

Then we would have to pattern match on `n` at the value level to
propagate information about `n` back to the type level.

If we did something like

``` haskell
    foo :: (n :: Int) -> (if n == 0 then Bool else ())
    foo n = case n of
      0 -> True
      _ -> ()
```

Then the typechecker would see that we're matching on `n`, so if we
get into the `0 -> ...` branch then `n` must be `0`. It can then
reduce the return type to `if 0 == 0 then Bool else ()` and finally
`Bool`. A very important thing to note here is that the typechecker
*doesn't evaluate the program*. It's examining the function in
isolation of all other values. This means we sometimes have to hold
its hand to ensure that it can figure out that all branches have the
correct type.

This means that when we use pi types we often have to pattern match on
our arguments in order to help the typechecker figure out what's going
on.

To make this clear, let's play the typechecker for this function. I'm
reverting to the `Nat` type since it's nicer for pattern matching.

``` haskell
    toRNat :: (n :: Nat) -> RNat n
    toRNat Z = RZ -- We know that n is `Z` in this branch
    toRNat (S n) = RS (toRNat n {- This has the type RNat n' -})

    p :: (n :: Nat) -> (m :: Int) -> RNat (plus n m)
    p Z m     = toRNat m
    p (S n) m = RS (toRNat n m)
```

First the type checker goes through `toRNat`.

In the first branch we have `n` equals `Z`, so `RZ` trivially typechecks. Next we have the
case `S n`.

 - We know that `toRNat n` has the type `RNat n'` by induction
 - We also know that `S n' = n`.
 - Therefore `RS` builds us a term of type `RNat n`.

Now for `p`. We start in much the same manner.

if we enter the `p Z m` case

 - we know that `n` is `Z`.
 - we can reduce `plus n m` since `plus Z m` is by definition equal to `m`
   Look at the definition of `plus` to confirm this).
 - We know how to produce `RNat m` easily since we have a function
 `toRNat :: (n :: Nat) -> RNat n`.
 - We can apply this to `m` and the resulting term has the type `RNat m`.

In the `RS` case we know that we're trying to produce a term of type
`RNat (plus (S n) m)`.

 - Now since we know that the constructor for the first argument of
 `plus`, we can reduce `plus (S n) m` to  `S (plus n m)` by the definition of `plus`.
 - We're looking to build a term of type `plus n m` and that's as
   simple as a recursive call.
 - From here we just need to apply `RS` to give us `S (plus n m)`
 - As we previously noted `S (plus n m)` is equal to `plus (S n) m`

Notice how as we stepped through this as the typechecker we never
needed to do any arbitrary reductions. We only ever reduce
definitions when we have the outer constructor (WHNF) of one of the
arguments.

While I'm not actually proposing adding `{-# LANGUAGE PiTypes #-}` to
GHC, it's clear that with only a few orthogonal editions to system F
we can get some seriously cool types.

## Wrap Up

Believe or not we've just gone through two of the most central
concepts in dependent types

 - Indexed type families (GGADTs)
 - Dependent function types (Pi types)

Not so bad was it? :) From here we'll look in the next post how to
translate our faux Haskell into actual Agda code. From there we'll go
through a few more detailed examples of pi types and GGADTs by poking
through some of the Agda standard library.

Thanks for reading, I must run since I'm late for class. It's an FP
class ironically enough.


[kind-exp]: /posts/2014-02-10-types-kinds-and-sorts.markdown
