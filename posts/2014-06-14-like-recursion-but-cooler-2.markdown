---
title: Grokking recursion-schemes: Part 2
---

In this post I'd like to talk about the second half of
recusion-schemes. [Previously](posts/2014-05-19-like-recursion-but-cooler.html)
we'd talked about catamorphisms and friends. These all focused on
"destroying" a datastructure by collapsing it layer by layer.

We're now going to talk about the opposite: anamorphisms. Anamorphisms
are just like generalized versions of `unfoldr`.

### Getting Anamorphisms
To demonstrate how to start anamorphisms, we'll create our custom list again.

``` haskell
    {-# LANGUAGE DeriveFunctor #-}
    data MyList a  = MyCons a (MyList a) | MyNil
    data ListB a b = BCons a b           | BNil
                   deriving Functor
```

Now we create an instance of the type class `Unfoldable` (shocker I know)

``` haskell
    type instance Base (MyList a) = BList a

    instance Unfoldable (MyList a) where
      embed (BCons a b) = MyCons a b
      embed BNil        = MyNil
```

That's it! We define the dual to `Foldable`'s `project`, `embed`. This just defines
how to take the datastructure that we've built up and stick it back into our list.

### Using Anamorphisms
Now, let's actually start writing some anamorphisms. The simplest example of an
unfolding I can think of is `between`. `between` takes two boundaries and then creates
a list of values between the high and the low, (low, high).

``` haskell
    > enum 1 5
      [2, 3, 4]
    > enum 'a' 'c'
      "b"
    > enum False False
      [False]
```

To make this more fun, we'll return `MyList a` instead of just `[a]` since it'll
make it easier to show off recursion-schemes. I'll explain how to generate `[a]`'s momentarily.

Now it's pretty obvious the type of `between` should be something like

``` haskell
    between :: (Eq a, Enum a) => a -> a -> MyList a
```

We could write this with simple, boring recursion

``` haskell
    between a b | a == b    = MyNil
                | otherwise = (succ a) `MyCons` enum (succ a) b
```

But this is exactly what we were avoiding! Let's rewrite this to use
an anamorphism. The type of `ana` (our anamorphism implementation) is

``` haskell
    ana :: (a -> Base t a) -> a -> t
```

This is the almost the exact opposite of `cata :: (Base t a -> a) -> t -> a`. So instead
of tearing the structure down layer by layer, we build it *up* layer by layer.

``` haskell
    between low high = ana builder low
      where builder a = ???
```

where `builder` is takes an `a` and returns the either `BCons (succ a) (succ a)`
or `BNil` if `a == high`. This is trivial to implement

``` haskell
    between low high = ana builder low
      where builder a | a == b    = BNil
                      | otherwise = join BCons (succ a) -- from Control.Monad
```

That's it! `builder` captures the essence of how we build up the list, one cons at a
time.

Now, as promised here's how to actually implement it so it returns `[a]`'s.

``` haskell
    between low high = ana builder low
      where builder a | a == b    = Nil
                      | otherwise = join Cons (succ a)
```

recursion-schemes defines the type instance for `[a]` with two constructor
`Cons` and `Nil` that behave precisely like `BCons` and `BNil`. However,
`Cons` and `Nil` are defined using some type families magic that makes them
invisible in the documentation (I found them by reading the source). They
exist I promise :)

Now, I said before this was just a generalized version of `unfoldr`,
let's look at the type of `unfoldr`.

``` haskell
    Data.List.unfoldr :: (b -> Maybe (a, b)) -> b -> [a]
```

So `unfoldr` takes our seed value, `b`, and splits it into either a
value and another seed, or nothing. Sound familiar? Look again at
`Cons`, `Cons` is a value `a`, and the next seed `b`! Furthermore
`Nil` is completely ismorphic to `Nothing` here.

Now `ana` generalizes upon `unfoldr` since we don't need to represent
everything as either 1 terminator, `Nothing`, or one builder, `(a,
b)`.

We could imagine something like

``` haskell
    data RedBlack a = Red   a (RedBlack a) (RedBlack a)
                    | Black a (RedBlack a) (RedBlack a)
                    | Leaf
```

Now `ana` could handle the fact that we can now "build" new seeds in
two ways, with `RedB` or `BlackB`!

### Building Stuff Up to Tear It Down
One of the most common patterns in Haskell is to create some
intermediate data structure and immediately use it.

This is kinda like smashing an anamorphism and a catamorphism together into
one. This has a name: a hylomorphism, `hylo` in recursion-schemes.

It turns out that this is one of the most useful applications of anamorphisms!

As a fun example, Daniel Wagner [blogged](http://mathlesstraveled.com/2008/01/07/recounting-the-rationals-part-ii-fractions-grow-on-trees/)
about how we
can generate an infinite list of all rational numbers. The key to this
is an infinite binary tree where each node is a rational number `p/q` and
it's two children are `(p + q) / q` and `p / (p + q)`.

We can build this binary tree with `ana`.

``` haskell
    import GHC.Real
  
    data Bin a    = Node a (Bin a) (Bin a)
    data BBin a b = NodeB a b b deriving Functor

    type instance Base (Bin a) = BBin a

    instance Unfoldable (Bin a) where
      embed (NodeB a l r) = Node a l r
    instance Foldable (Bin a) where
    

    rats :: Bin Rational
    rats = ana builder (1 % 1)
      where builder r@(p :% q) = NodeB r ((p + q) % q) (p % (p + q))
```

We can collapse it into a list with `cata`

``` haskell
    collapse :: Bin a -> [a]
    collapse = cata folder
     where folder (NodeB a l r)         = a : interleave l r
           interleave (x : xs) (y : ys) = x : y : interleave xs ys
```

The work horse here is `interleave` which just describes how to safely combine
two infinite lists.

Now we can combine the process of building up our binary tree and generating a list
into one cool transformation

``` haskell
    allRats :: [Rational]
    allRats = hylo folder builder (1 % 1)
      where folder (NodeB a l r)         = a : interleave l r
            interleave (x : xs) (y : ys) = x : y : interleave xs ys
            builder r@(p :% q)           = NodeB r ((p + q) % q) (p % (p + q))
```

There you are! As a challenge to the reader, figure out what
index a number `p/q` will appear in this list (it will only occur once).

If you found this math intersting, check out
[this paper](http://www.cs.ox.ac.uk/jeremy.gibbons/publications/rationals.pdf).

A few other people have shown off this pattern, one of my favorites
being [merge sort as a hylomorphism](http://fho.f12n.de/posts/2014-05-07-dont-fear-the-cat.html).

## A Recap

We've now covered the core elements of the `recursion-schemes` library, but I'm not
quite done with this blog series. I'm planning on one more post detailing my attempt
to actually use recursion-schemes in a real project: a scheme compiler.

I think it
would make the post more interesting though if the next post didn't just include
an example of "stuff I find cool", so, if you have any particular example of cleaning
up some code using recursion-schemes, please let me know! I'd love to share any
and all examples I can find since that's been the best way I've found to actually
grok `recurion-schemes`.

If you're interested in sharing, either comment or email me at jozefg [at] cmu.edu.

*Thanks to tel for proof reading*
