---
title: Fixpoints and Iso-recursive Types
tags: haskell, types
---

Let's imagine a world where Haskell didn't have recursive functions.
The Haskell committee simply left them out by mistake. Could Haskell still
be Turing complete?

Well we need some method of arbitrary recursion, let's try what we'd do
in lambda calculus: fixpoints. In particular, we want a function like this

```haskell
    fix :: (a -> a) -> a
```


### What are fixpoints?
So we pass in a function `f`, and it will return to us a value `a`
so that `f a = a`. Why is this useful? Imagine we encode recursive functions like
this

```haskell
    factorial :: (Int -> Int)
              -> Int
              -> Int
    factorial self 0 = 1
    factorial self n = n * self (n-1)
```

So we pass along recursion through this extra function. Well here factorial
is really a function of type

```haskell
    factorial :: (Int -> Int) -> (Int -> Int)
```

And we want to fill it like this

```haskell
    completeFactorial n = factorial (factorial (factorial (factorial ...))) n
```

Now when we take the fixpoint, we find an `a` where `factorial a = a`. This means that

    factorial a = factorial (factorial a) = factorial (factorial (factorial ...)))

So with fixpoints, we get that infinitely long chain that we wanted.

### Fixpoints with recursion
If we allowed ourselves recursion for a moment then `fix` is easy

    fix f = let x = f x in x

This looks silly, but in fact, if `f` isn't strict in `x`, than `x` can
be non-bottom. And if `f` is strict than by definition, `f ⊥ = ⊥` so `⊥` is
a fixpoint.

### Fixpoints without recursion
Now we can actually still write a fixpoint-finding function without recursion.
The most famous one is the y-combinator. In lambda calculus, we'd write this as

    Y = λf . (λx . f (x x)) (λx . f (x x))

And we want to show that `Y f = f (Y f)`

    Y f = f (Y f)
    (λx . f (x x)) (λx . f (x x)) = f ((λx . f (x x)) (λx . f (x x)))
    f((λx . f (x x)) (λx . f (x x))) = f ((λx . f (x x)) (λx . f (x x)))

With simple beta reduction, they're equal.

#### Fixpoints in Haskell
Now in Haskell, this doesn't work. We could try

```haskell
    fix f = (\x -> f (x x)) (\x -> f (x x))
```

But what is `x`'s type? Well it's a function so

    x :: a -> b

And `x`'s first argument is `x`, so

```haskell
    type T = T -> b
    type T = μR. R -> b -- This means the same as the above
    x :: T
```

But this isn't legal! We can't have infinite types like that. Don't
despair though, we're going to use the magic of iso-recursive types.

What are iso-recursive types? Well they're like (equi-)recursive types, but
they provide two operations, `fold` and `unfold`.

```haskell
    unfold :: μX. T -> [μX. T/X]T
    fold   :: [μX. T/T]T -> μX. T
```

Where `[foo/bar]baz` means, "substitute all occurrences of `bar` with `foo` in
`baz`. When trying to unify iso-recursive types, we don't consider a type equal
to an unfolding of that type. This makes type inference considerably easier since
we're requiring the user to explicitly fold and unfold types.

We can write these in Haskell

```haskell
    newtype Mu f = Mu {unMu :: f (Mu f)}
    unfold = unMu
    fold   = Mu
```

Now we can write the type of `x`

```haskell
    newtype X' b a = {unX :: a -> b}
    type X a = Mu (X' a)
```

Take a moment to think about this, mentally unfolding we have

    X a
    Mu (X' a)
    Mu X' -> a
    (Mu X' -> a) -> a
    ...

There we go! Now for that y combinator

```haskell
    unfold' = unX  . unfold
    fold'   = fold . X'
    y f = (\x -> f (unfold' x x)) $ fold' (\x -> f (unfold' x x))
```

and finally

```haskell
    fix = y
```

And to test it

```haskell
    f = fix factorial -- From the way before
    main = mapM_ (print . f) [1..5]
```

prints

    1
    2
    6
    24
    120

Which means we're successful, we've added back recursion to Haskell!
