---
title: Sieves in Haskell: Part 2
---
So in my last post about sieves in Haskell, I'd mentioned that there is a purely
functional approach infinite, lazy sieves. In the post I'll explain how to construct
these.

The first step to implementing the lazy version is to realize we need 2 kinds of laziness,

 1. Laziness in the actual list
 2. Laziness in how we cross things off

This is important because in order to maintain an efficient sieve, we must cross
off all multiples of a number when we see the number, otherwise we degrade to
trial division.

However, this seems impossible since well, our list is infinitely long. We can work
around this though by using a form of iterators.

Specifically, we'll have a function like this


``` haskell
    import qualified Data.IntMap.Strict as M
    import Data.List
    sieve' :: [Int] -> M.IntMap [Int] -> [Int]
```

And in that `IntMap`, we store a list of prime factors of that number.
We use these as "iterators". So when we get to a number `n`,
if `n` has a list of primes associated with them, insert each
prime `p` at `p + n` in the map and don't add `n` to our list.
If `n` has no prime factors, then it is clearly prime so we add
it to our list and add `n` as a prime factor of `2 * n`.

In Haskell code

``` haskell
    sieve' (n:ns) m =
      case M.lookup n m of
        Nothing -> n : sieve' ns (M.insertWith (++) (2 * n) [n] m)
        Just ps -> sieve' ns $ foldl' insertPrime (M.delete n m) ps
      where insertPrime m p = M.insertWith (++) (n + p) [p] m
```

And then to drive this, we can just use

``` haskell
    sieve :: [Int]
    sieve = sieve' [2..] M.empty

    main = sum . takeWhile (<2000000) $ sieve
```

And there you have it. This code is certainly cleaner than the `ST` version and decently
performant, clocking in at 3 seconds. This is a little unfair to this version though
since the `ST` code takes advantage of unboxed types which isn't possible here. Additionally,
this version is strictly more general, creating an infinite list rather than a finite one.

I'll updated with a fairer benchmark once I have time to redo the `ST` code.
