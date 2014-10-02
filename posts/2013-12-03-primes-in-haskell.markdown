---
title: Sieves in Haskell
tags: haskell
---

The other day I was answering a question on StackOverflow and decided
the solution was worth talking about.

It was particularly interesting because it illustrated something: Haskell
makes a darn good imperative language.

What do I mean? This statement seems absurd, Haskell has no notion
of state! How could it be used for imperative programming?

Well thanks to monads and `do` notation, we're going to translate the
following Python code to Haskell

``` python
    def sieve(n):
        nums = [True for _ in range(n)]
        nums[0] = False
        nums[1] = False
        for i in range(n):
            if nums[i]:
               for mul in range(i*2, n, i):
                   nums[mul] = False
        return [i for i, v in enumerate(nums) if v]
```

This is the sieve of Eratosthenes, it works like this

 1. Write out the nums 0 - n
 2. Cross off 0 and 1
 3. For the next not crossed off number, cross of its multiples
 4. Repeat 3 until the end of the list
 5. The remaining numbers are primes

So in Python, we write this with a mutable list, `nums`. Then we
just loop through each index and cross of as we go along! It's
a pretty straightforward translation.

Now we could write this in pure Haskell,

```
    sieve n = go 2 $ False : False : replicate (n-2) True
      where go = ...
```

But this is incredibly awkward and inefficient, since updating an
element takes both linear time and space. We could opt for a clever
solution

``` haskell
    primes = go [2..]
      where go (p:rest) = p : [r | r <- rest, r `mod` p /= 0]
    sieve = flip takeWhile primes . (<)
```

But this is still very, very inefficient. And in fact, it's not even a sieve!
it's called trial division.

So, what's a functional programmer to do.. Well, let's start by cheating!

## The ST Monad

``` haskell
    import Data.Vector.Unboxed hiding (forM_)
    import Data.Vector.Unboxed.Mutable
    import Control.Monad.ST (runST)
    import Control.Monad (forM_, when)
    import Prelude hiding (read)
```
This laundry list of imports gives us access to something pretty cool:
mutable state in Haskell. `ST` is short for State Thread and acts as
a safe wrapper around `IO`. It'll let us imperative code, mutate things,
but then force us to present a pure interface!

The purity trick is actually quite clever, it's done by providing an
escape hatch out of `ST` with the type

``` haskell
    runST :: (forall s. ST s a) -> a
```

So we can only escape `ST` when this phantom `s` is universally quantified.
This forces you handle the `s`, the state, opaquely which prevents us from
doing anything unsafe.

Next we can use `Data.Vector.Unboxed.Mutable` to create unboxed
mutable vectors.

``` haskell
    new :: Int -> ST s (MVector s a)
```
And now all of this leads to

``` haskell
    sieve :: Int -> Vector Bool
    sieve n = runST $ do
      vec <- new (n + 1) -- Create the mutable vector
      set vec True       -- Set all the elements to True
      forM_ [2..n] $ \ i -> do -- Loop for i from 2 to n
        val <- read vec i -- read the value at i
        when val $ -- if the value is true, set all it's multiples to false
          forM_ [2*i, 3*i .. n] $ \j -> write vec j False
      freeze vec -- return the immutable vector
```

Using this we can easily sum the first 10k primes (project Euler 10)

``` haskell
    main = print . ifoldl' summer 0 $ sieve 2000000
      where summer s i b = if b then i + s else s
```

So there you have the "cheater" way. But darn it it's fast, that sums
the first 10,000 primes in around 0.2 seconds.

In my next post, I'll explain a lazy functional way to do this to produce an infinite sieve.
