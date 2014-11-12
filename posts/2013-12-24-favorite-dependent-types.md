---
title: Some Fun Dependently Typed Programs
tags: agda, types
---

I've been playing with Agda lately and decided to translate my two favorite dependently typed
programs from Coq to Agda. Here are the results.

## Variadic Functions
Variadic functions are hard to do right. Especially since with `fun a b c d e` it isn't
clear if `fun a b c d` is supposed to be a call to a variadic function which returns
a function, or whether the whole thing is one function call, or just a type error.

Now it'd be nice if we could say something like

    add 7 1 2 3 4 5 6 7

In other words, tell the variadic function how many arguments we'll give it at runtime.
This is marginally less flexible than just listing them off, but not by much.

Now how could we encode this? Our type would *depend* on the value we pass it! So
we could write something like

``` agda
    open import Data.Nat

    var_ty : ℕ -> Set -> Set -> Set
    var_ty 0 _  ret    = ret
    var_ty (suc a) t r = t -> var_ty a t r
```

So a call to `var_ty` returns a function of `n` arguments
of type `t` and returns an `r`.

From there it's simple to write our variadic sum,

``` agda
    var_sum' : (n : ℕ) -> ℕ -> var_ty n ℕ ℕ
    var_sum' 0 cur       = cur
    var_sum' (suc a) cur = \x -> var_sum' a (cur + x)

    var_sum : (n : ℕ) -> var_ty n ℕ ℕ
    var_sum n = var_sum' n 0
```

And `var_sum 3 1 2 3` evaluates to `6`, kinda nifty.

## Heterogeneous lists

Functional languages are commonplace in functional programming, but they're
almost always homogeneous, meaning the list has only one type of element.

It'd be nice to store multiple types in the same list, and indeed we can do this
with a bit of dependent-type-foo

``` agda
    open import Data.List

    data HList : List Set -> Set1 where
      []  : HList []
      _∷_ : {A : Set}{xs : List Set} -> A -> HList xs -> HList (A ∷ xs)
```

So we attach a list of types to our `HList` and the element at `i` has the
type of this list at `i`. The interesting bit is the definition of `∷`.
It takes an implicit type `A`, and a list of types `xs`. It then takes
an `A` and an `HList xs` and returns the new updated `HList`.

This is quite similar to `(a, (b, (c, ())))` in Haskell, but the dependent
types make it much more pleasant to use. For example, we can now write

    foo : HList _
    foo = 1 ∷ true ∷ "foo" ∷ tt ∷ []

Not too shabby. This is much more pleasant to use than the Haskell equivalent,
nested tuples. For example, to write a length function for tuples in Haskell,
you'd have to say something like

``` haskell
    class HasLength a where
      ...
    instance HasLength b => HasLength (a, b) where
      ...
```

And rely on some typeclass prolog. Compare this to the equivalent

``` agda
    hlength : {xs : List Set} -> HList xs -> ℕ
    hlength {xs} _ = length xs
```

Since we have a nice flat list of all the types in our
structure, it's much simpler to work with.

That's all for now, I'll probably rant more about agda in the future.
