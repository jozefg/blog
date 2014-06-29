---
title: Some Useful Agda
---

I've been using Agda for a few months now. I've always meant to figure
out how it handles IO but never have.

Today I decided to change that! So off I went to the related Agda
[wiki page](http://wiki.portal.chalmers.se/agda/pmwiki.php?n=ReferenceManual2.Compilation).
So hello world in Agda apparently looks like this

``` agda
    open import IO
    
    main = run (putStrLn "test")
```

The first time I tried running this I got an error about an `IO.FFI`,
if you get this you need to go into your standard library and run
`cabal install` in the `ffi` folder.

Now, on to what this actually does. Like Haskell, Agda has an `IO`
monad. In fact, near as I can tell this isn't a coincidence at all,
Agda's primitive IO seems to be a direct call to Haskell's IO.

Unlike Haskell, Agda has two IO monads, a "raw" primitive one and a
higher level pure one found in `IO.agda`. What few docs there are
make it clear that you are not intended to write the "primitive IO".

Instead, one writes in this higher level `IO` monad and then uses a
function called `run` which converts everything to the primitive IO.

So one might ask: what exactly is this strange `IO` monad and how does
it actually provide `return` and `>>=`? Well the docs don't actually
seem to exist so poking about the source reveals

``` agda
    data IO {a} (A : Set a) : Set (suc a) where
      lift   : (m : Prim.IO A) → IO A
      return : (x : A) → IO A
      _>>=_  : {B : Set a} (m : ∞ (IO B)) (f : (x : B) → ∞ (IO A)) → IO A
      _>>_   : {B : Set a} (m₁ : ∞ (IO B)) (m₂ : ∞ (IO A)) → IO A
```

Wow.. I don't know about you, but this was a bit different than I was
expecting.

So this actually just forms a syntax tree! There's something quite
special about this tree though, those ∞ annotations mean that it's a
"coinductive" tree. So we can construct infinite `IO` tree. Otherwise
it's just a normal tree.

Right below that in the source is the definition of `run`

``` agda
    {-# NO_TERMINATION_CHECK #-}
    run : ∀ {a} {A : Set a} → IO A → Prim.IO A
    run (lift m)   = m
    run (return x) = Prim.return x
    run (m  >>= f) = Prim._>>=_ (run (♭ m )) λ x → run (♭ (f x))
    run (m₁ >> m₂) = Prim._>>=_ (run (♭ m₁)) λ _ → run (♭ m₂)
```

So here's where the evilness comes in! We can loop forever
transforming our `IO` into a `Prim.IO`.

Now I had never used Agda's coinductive features before and if you
haven't either than they're not terribly complicated.

`∞` is a prefix operator that stands for a "coinductive computation"
which is roughly a thunk. `♯` is a prefix operator that delays a
computation and `♭` forces it.

There are reasonably complex rules that
govern what qualifies as a "safe" way to force things. Guarded
recursion seems to always work though. So we can write something like

``` agda
    open import Coinduction
    open import Data.Unit
    
    data Cothingy (A : Set) : Set where
      conil  : Cothingy A
      coCons : A → ∞ (Cothingy A) → Cothingy A
    
    lotsa-units : Cothingy ⊤
    lotsa-units = coCons tt (♯ lotsa-units)
```

Now using ♯ we can actually construct programs with infinite output.

``` agda
    forever : IO ⊤
    forever = ♯ putStrLn "Hi" >> ♯ forever

    main = run forever
```

This when run will output "Hi" forever. This is actually quite
pleasant when you think about it! You can view you're resulting
computation as a normal, first class data structure and then reify it
to actual computations with `run`.

So with all of this figured out, I wanted to write a simple program in
Agda just to make sure that I got it all.

## FizzBuzz

I decided to write the fizz-buzz program. For those unfamiliar, the
specification of the program is

> For each of the numbers 0 to 100, if the number is divisible by 3
> print fizz, if it's divisible by 5 print buzz, if it's divisible by
> both print fizzbuzz. Otherwise just print the number.

This program is pretty straightforward. First, the laundry list of
imports

``` agda
    module fizzbuzz where
    
    import Data.Nat        as N
    import Data.Nat.DivMod as N
    import Data.Nat.Show   as N
    import Data.Bool       as B
    import Data.Fin        as F
    import Data.Unit       as U
    import Data.String     as S
    open import Data.Product using (_,_ ; _×_)
    open import IO
    open import Coinduction
    open import Relation.Nullary
    open import Function
```

This seems to be the downside of finely grained modules.. Tons and
tons of imports.

Now we need a function which takes to ℕs and returns true if the first
mod the second is zero.

``` agda
    congruent : N.ℕ → N.ℕ → B.Bool
    congruent n N.zero    = B.false
    congruent n (N.suc m) with N._≟_ 0 $ F.toℕ (N._mod_ n (N.suc m) {U.tt})
    ... | yes _ = B.true
    ... | no  _ = B.false
```

Now from here we can combine this into the actual worker for the
program

``` agda
    
    _and_ : {A B : Set} → A → B → A × B
    _and_ = _,_
    
    fizzbuzz : N.ℕ → S.String
    fizzbuzz N.zero    = "fizzbuzz"
    fizzbuzz n with congruent n 3 and congruent n 5
    ... | B.true  , B.true   = "fizzbuzz"
    ... | B.true  , B.false  = "fizz"
    ... | B.false , B.true   = "buzz"
    ... | B.false , B.false  = N.show n
```

Now all that's left is the `IO` glue

``` agda
    worker : N.ℕ → IO U.⊤
    worker N.zero    = putStrLn $ fizzbuzz N.zero
    worker (N.suc n) = ♯ worker n >> ♯ putStrLn (fizzbuzz $ N.suc n)
    
    main = run $ worker 100
```

There. A somewhat real, IO based program written in Agda. It only took
me 8 months to figure out how to write it :)
