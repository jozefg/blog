-----
title: 5 things I hate about Haskell
-----
I was on programmers.stackexchange today and I say an interesting question,
[Why the question “give five things you hate about C#” is so difficult to answer during an interview?](http://programmers.stackexchange.com/questions/159754/why-the-question-give-five-things-you-hate-about-c-is-so-difficult-to-answer). It got me thinking, can I name 5 things I hate about my Haskell, my favorite language?

Well the short answer is, Yes and here they are

### 1. Prelude

I know exactly *why* `map`, `foldl`, and friends are monomorphic,
it doesn't stop me from muttering about having to import `Data.Foldable` and hide a bunch of stuff.

### 2. Modules

Haskell's modules are in essence the barest minimum needed to be a module system. I which we had
 first class modules, ML-style functors, etc etc

### 3. Records

This has been talked to death... I don't like records, they're pretty hacky. Happily `lens` pretty
 much works wherever I'd normally use records. This may improve after the GSoC project for generic
 records functions gets merged.

### 4. Monomorphism

I don't mind the monomorphism restriction, but I do wish that the errors it generates on GHC referenced the monomorphic variable rather
 than saying "can't unify "Foo" with "Bar"". I'm well aware that a `Foo` isn't a `Bar`, but seeing why some type var magically became `Foo`
 would be helpful. Also, it'd be nice if when mentioned, monomorphic variables were different syntactically than
 normal onces, perhaps prefixed with ! or _.

### 5. `fail`

I really wish that `fail` wasn't part of the `monad` specification. Everywhere else in Haskell, if you don't provide the appropriate
 case to a pattern match, you get a runtime failure, fine. But in a monad, you might just end up with `fail`. Which is *probably* a runtime
 exception, but might also be a parser failure or `Nothing` or `Left` or something else just to name a few. Ick. I'd much prefer it if
 we just had runtime failures and then used `mzero` when we want failure. That's what it's for.
 
 
