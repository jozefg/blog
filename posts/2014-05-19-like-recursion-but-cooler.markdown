----
title: Grokking recursion-scheme: Part 1
----

This post is a little different than the rest of my blog, I'm
not nearly as competent with [recursion-schemes](https://hackage.haskell.org/package/recursion-schemes)
as I want to be and I
don't understand them fully (yet). This isn't entirely complete,
but I hope it will provide a useful intuition for how to work with
some of the lower ends of recursion-schemes and some idea of how to
get into the higher end. I'll be reading this again in two weeks
once I've forgotten all of this (again). You've been warned...

### Why Bother?
First, let's talk about why anyone would care about using a library
like recursion-schemes.

Remember back in the good old days when all
a programmer was `goto` and guts? And everyone hated it? We're at
a not dissimilar place in Haskell. Well, it's not nearly so bad nowadays,
however, our principle form of control flow is recursion and
really we mostly use recursion in a raw, unprincipled way.

However, we're starting to move away from it. Do these
look familiar?

``` haskell
    foldr :: (a -> b -> b) -> b -> [a] -> b
    foldr f nil (x : xs) = x `f` foldr f nil xs
    foldr f nil []       = nil
```

`foldr` is all about abstracting away raw recursion! `foldr` is great
in this way since it covers a surprisingly large cover of cases

``` haskell
    map :: (a -> b) -> [a] -> [b]
    map f = foldr ((:) . f)

    filter :: (a -> Bool) -> [a] -> [a]
    filter p = foldr (\x rest -> if p x then x : rest else rest)
```

Turns out you can implement quite a lot of Data.List with `foldr`
and poor judgment.

However, this isn't good enough. For example, I do a lot of work
with compilers and therefore spend a lot of time doing transformations
on trees. I want something like `foldr` to deal with this.

recursion-schemes is one such option. It's a way of generalizing
these uniform transformations on structures and it's expanded to
cover *a lot* transformations.

### On to recursion-schemes
Now that we know that recursion-schemes is solving
a useful problem, let's get into actually using it.
First, we can install it off of hackage

    cabal install recursion-schemes

And import everything with

``` haskell
    {-# LANGUAGE TypeFamilies, DeriveFunctor #-}
    import Data.Functor.Foldable
```

Let's get started by seeing how recursion-schemes covers
`foldr`

First, we define our own custom list

``` haskell
    data MyList a = MyCons a (MyList a) | MyNil
```

Next, we define another type of list, with
the recursion factored out

``` haskell
    data BList a b = BCons a b | BNil
         deriving Functor
```

Here `b` is the recursive bit of `BList` factored out
into an explicit parameter. So

``` haskell
    MyList a ~ BList a (BList a (BList a ....))
```

The fancy term for this would be to say that `List a`
is the "fixed point" for `BList a`.

Now we can actually use recursion-schemes

``` haskell
    type instance Base (List a) = BList a

    instance Foldable (List a) where
      project (MyCons a b) = BCons a b
      project MyNil        = BNil
```

And we're done. So to understand what's going on
we need to talk about another data type and a little
math.

``` haskell
    newtype Fix f a = Fix {unFix :: f (Fix f a)}
```

Remember before how I mentioned how `MyList` is the
fixed point of `BList`? Well `Fix` let's us exploit
this fact. In particular

``` haskell
    out :: Fix (BList a) -> MyList a
    out (Fix (BCons a rest)) = MyCons a (out rest)

    into :: MyList a -> Fix (BList a)
    into (MyCons a rest) = Fix (BCons a $ into rest)
```

So we could write either `BList` or `MyList` for all our
data types, but the `BList` version is really a pain to write since
everything is wrapped in `Fix`. Unfortunately though, it's much easier
to write generic code for stuff of the form `Fix (f a)`.

To solve this recursion-schemes has the type class `Base` where we
map the recursive data type to its non-recursive friend. Then,
in `project` we define how to.. well.. project the recursive into
a partially unfolded equivalent.

With just those two steps, we get a large chunk of recursion-schemes
operations for our data type!

### Just What Did We Get?
Now this was the part I really had trouble with in recursion-schemes
the names of the functions for `Foldable` are... opaque if you're
not familiar with the terminology.

The most basic one is `cata`, which is the "catamorphism" across
our data type. I'm not going to trouble you with why we call it a
catamorphism, but just remember that it's the souped-up version of `foldr`.

``` haskell
    foldr :: (a -> b -> b)          -> b -> [a]    -> b
    foldr :: ((a, b) -> b)          -> b -> [a]    -> b
    cata  :: (Fix BList a -> b)     -> List a      -> b
    cata  :: (Base (List a) a -> b) -> List a      -> b
    cata  :: (Base t b -> b)        -> t           -> b
```

And we can use it the same way!

``` haskell
    map :: (a -> b) -> List a -> List b
    map f = cata mapper
      where mapper (BCons a b) = f a `MyCons` b
            mapper BNil        = MyNil
            

    myfilter :: (a -> Bool) -> List a -> List a
    myfilter p = cata filterer
      where filterer (BCons a b) = if p a then a `MyCons` b else b
            filterer BNil        = MyNil
```

Now we can all tell people that we've written map using a catamorphism.

Careful readers will notice one big difference between `foldr` and `cata`:
`cata` doesn't take a seed! Indeed with `foldr` we replace all the constructors
of our list with the function `f`, so

``` haskell
    1 : 2 : 3 : 4 : []
    1 `f` 2 `f` 3 `f` 4 `f` seed
```

This doesn't generalize well though, what if we have a type with a constructor
of 3 arguments? Or 5? To avoid this problem, recursion-schemes takes a clever approach.

Remember that `BList` factors out recursion? `cata` works by collapsing a sublist recursively
and sticking the slot back into the slot of the original list. So we actually have something like

``` haskell
    BCons 1 (BCons 2 (BCons 3 (BCons 4 BNil)))
    BCons 1 (f (BCons 2 (f (BCons 3 (f (BCons 4 (f Nil)))))))
```

Now `f` has to handle all possible cases of our constructor, so it handles both the
seed value and the collapsing case! And this generalizing beautifully by just delegating
all the constructor specific work to `f` this is how it's possible to derive `cata` practically
for free.

Now, since recursion-schemes already has an instance for `[a]`, I'll dispense
with `MyList` since it's a bit clunky.

Our foldable instance gives us quite a bit more than just `foldr` however!
We also get this function `para`, short for "paramorphisms". A paramorphism
is like a fold, but also gives a "snapshot" of the structure at the point we're
folding. So if we wanted to sum each tail of a list, we could do something like

``` haskell
    sumTails :: Num a => [a] -> [a]
    sumTails = para summer
      where summer (Cons a (list, rest)) = a + sum list : rest
            summer Nil                   = []
```

This could be useful for example, if you're doing any context dependent
operations on a structure. Later, I'll try to include some more practical
examples of a paramorphism (I never thought I'd say those words).

Now recursion-schemes includes *generalized* versions of all of these
but I'm not brave enough to try to explain them right now.

### A Real Example
Before we wrap this post up, let's demonstrate an actual useful example of
recursion-schemes.

We're going to implement trivial constant folding in a made up language
I'll call Foo.

The AST for Foo is something like

``` haskell
    data Op = Plus | Sub | Mult | Div

    data Foo = Num Int           -- Numeric literals
             | String String     -- String literals
             | Binop Op Foo Foo  -- Primitive operation
             | Fun String Foo    -- Lambda/Abstraction over terms
             | App Foo Foo       -- Application
             | Var String        -- Variables
             deriving Show
```

Now we want our trivial constant folding to reduce something
like `Binop Plus (Num 1) (Num 2)` to just `Num 3`. Let's
first formalize this by writing a quick little reducer

``` haskell
    compute :: Op -> Int -> Int -> Int
    compute Plus = (+)
    compute Sub  = (-)
    compute Mult = (*)
    compute Div  = div

    reduce :: Foo -> Foo
    reduce (Binop op (Num a) (Num b)) = Num $ compute op a b -- The reduction
    reduce a                          = a
```

So we compute all constant expressions and leave everything else alone.
This is pretty simple, but how can we apply it to *every* element in
our AST? Well, time to break out recursion-schemes

``` haskell
    data FooB a = NumB Int
                | StringB String
                | BinopB Op a a
                | FunB String a
                | App a a
                | Var String
    type instance Base Foo = FooB
```

And let's rewrite `reduce` to use `FooB` instead of `Foo`

``` haskell
    reduce :: Base Foo Foo -> Foo
    reduce (Fix (Binop op (Num a) (Num b))) = Num $ compute op a b -- The reduction
    reduce a                                = a
```

So this entire traversal now just becomes

``` haskell
    constFold :: Foo -> Foo
    constFold = cata reduce
```

Now we can test our simple optimization

``` haskell
    test = Binop Plus (Num 1) (Binop Mult (Num 2) (Num 3))
    optimized = constFold test
    main = print test
```

As we'd hope, this prints out `Num 7`!

This seems like a lot of work but don't forget, now that we've
taught recursion-schemes how to do traversals, we get all of this
for free. For example, let's now write a function to grab all
the free variables of an expression.

As before, let's start by writing the simple worker
function for this traversal.

``` haskell
     freeVar :: Base Foo [String] -> [String]
     freeVar (NumB _)         = []
     freeVar (StringB _)      = []
     freeVar (VarB s)         = [s]
     freeVar (BinopB _ v1 v2) = v1 ++ v2
     freeVar (AppB v1 v2)     = v1 ++ v2
     freeVar (FunB v vs)      = delete v vs
```

Now the full traversal is trivial!

``` haskell
    freeIn :: Foo -> [String]
    freeIn = cata freeVars
```

As we'd hope, this traversal is much easier to write than the first one.
You can imagine that the boilerplate of writing `FooB` and `project` is
amortized over each traversal, making it much easier to write subsequent
traversals once we've gone through the trouble of actually laying down
the foundation.

### What's Next?

So far I've discussed part of the `Foldable` half of the recursion-schemes
library. In my next post I'll cover anamorphisms and `Unfoldable`, the
dual of what we've talked about here.
