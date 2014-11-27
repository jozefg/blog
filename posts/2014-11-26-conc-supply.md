---
title: Examining Hackage: concurrent-supply
tags: haskell
---

It's been a while since I posted about some code I've been reading,
but today I found a little gem:
[concurrent-supply][hackage-link]. This package sets out to provide
fast way to generate unique identifiers in a way that's splittable and
supports concurrency.

What's particularly cool about this package is that the code is only
about ~100 lines and a goodly chunk of that is pragramas to tell GHC
to actually inline trivial functions.

The API is just 5 functions

``` haskell
    type Supply
    newSupply :: IO Supply
    freshId :: Supply -> (Int, Supply)
    splitSupply :: Supply -> (Supply, Supply)

    freshId# :: Supply -> (# Int, Supply #)
    splitSupply# :: Supply -> (# Int, Supply #)
```

`Supply` is the type for well.. supplies of fresh integers. We can
grab an `Int` out of a supply producing a new supply as well. We can
also split a supply so that we have two new supplies that will produce
disjoint identifiers.

The idea here is that we can have supplies that are used from multiple
concurrent threads and they won't ever

 1. Duplicate identifiers between supply
 2. Hammer on the same supply and destroy all our concurrency

It does go without saying that eventually we run out of ints, so I
suppose if you sit and prod a supply for a very long time, *something*
bad will happen.

With that in mind, let's take a look at the imports for
`Control.Concurrent.Supply`.


``` haskell
    import Data.Hashable
    import Data.IORef
    import Data.Functor ((<$>))
    import Data.Monoid
    import GHC.IO (unsafeDupablePerformIO, unsafePerformIO)
    import GHC.Types (Int(..))
    import GHC.Prim (Int#)
```

So you can see that some interesting stuff is going to happen, we have
both unboxed ints, and `unsafe*PerformIO`s. As a quick review,
`unsafeDupablePerformIO` is for `IO` actions which are okay being
forced at the same time by different threads which `unsafePerformIO`
is a little bit more modest and ensures we only force things from one
thread at a time.

With this in mind, the code starts with the classic definition of
streams in Haskell.

``` haskell
    infixr 5 :-
    data Stream a = a :- Stream a
```

This is followed with some rather a few definitions,

``` haskell
    instance Functor Stream where
      fmap f (a :- as) = f a :- fmap f as

    extract :: Stream a -> a
    extract (a :- _) = a

    units :: Stream ()
    units = () :- units
    {-# NOINLINE units #-}
```

Do note that `units` won't be inlined, this is unfortunately important
when we're thinking about with unsafe functions.

Now on top of streams we can define a rather important type, blocks.

``` haskell
    data Block = Block Int !(Stream Block)

    instance Eq Block where
      Block a (Block b _ :- _) == Block c (Block d _ :- _) = a == c && b == d

    instance Ord Block where
      Block a (Block b _ :- _) `compare` Block c (Block d _ :- _) = compare a c `mappend` compare b d

    instance Show Block where
      showsPrec d (Block a (Block b _ :- _)) = showParen (d >= 10) $
        showString "Block " . showsPrec 10 a . showString " (Block "
                            . showsPrec 10 b . showString " ... :- ...)"

    instance Hashable Block where
      hashWithSalt s (Block a (Block b _ :- _)) = s `hashWithSalt` a `hashWithSalt` b
```

So a block is an integer and an infinite number of other
blocks. Notice that block identity is purely determined by the first
two ints. This is contingent on the fact that all blocks are made with

``` haskell
    blockSize :: Int
    blockSize = 1024
    {-# INLINE blockSize #-}

    -- Minimum size to be worth splitting a supply rather than
    -- just CAS'ing twice to avoid multiple subsequent biased splits
    blockCounter :: IORef Int
    blockCounter = unsafePerformIO (newIORef 0)
    {-# NOINLINE blockCounter #-}

    modifyBlock :: a -> IO Int
    modifyBlock _ =
      atomicModifyIORef blockCounter $ \ i ->
        let i' = i + blockSize in i' `seq` (i', i)
    {-# NOINLINE modifyBlock #-}

    gen :: a -> Block
    gen x = Block (unsafeDupablePerformIO (modifyBlock x)) (gen <$> units)
    {-# NOINLINE gen #-}

    newBlock :: IO Block
    newBlock = return $! gen ()
    {-# NOINLINE newBlock #-}
```

This is the first bit of unsafe code, so let's look at what's going
on. We have a normal constant `blockSize` which represents something,
it's not immediately clear what yet. There's a global mutable variable
`blockCounter` starting from zero. From there, we have `gen` which
creates a block by making a thunk which unsafely bumps the block
counter by 1024, returning its previous size. To get the stream of
blocks we `fmap` `units`.

It's worth wondering why we need this polymorphic argument. I'm
reasonable certain it's to prevent GHC from being clever and sharing
that `(unsafeDupablePerformIO ...)` between blocks. That would be very
bad. It might not do that if we where to use `()` instead of `a` but
there's no reason a future optimization (if it doesn't exist already)
wouldn't figure out that there's only one possible result type and
reduce the whole thing to a CAF.

Now a `newBlock` wraps all this unsafe updating in `IO` and returns the
application of `gen ()`.

So what does all of this mean? Well each block thunk is going to have
its own unique ID, separated by 1024 and only claimed whenever we
actually force its first component. We have this gnarly chunk of
mutable shared memory that we only ever modify with
`atomicModifyIORef`, we actually touch it whenever we inspect the
first thunk in a `Block`. What's particularly interesting is that this
can happen in pure code! By putting off this costly operation as long
as possible we amortize the cost of all that contention.

Now we also have to support split, luckily it's easy to split blocks
since we have an infinite number of them nested!

``` haskell
    splitBlock# :: Block -> (# Block, Block #)
    splitBlock# (Block i (x :- xs)) = (# x, Block i xs #)
```

It becomes a bit clearer now why we can completely determine blocks by
their "first two" elements. The head is completely unique to each
sequence so we know at minimum that if `i == j` in `Block i xs` and
`Block j ys` then either `xs` or `ys` is the tail of the other. This
is an invariant we maintain throughout the code not exposing `Block`
and by ensuring we never `:-` any new ones onto its internal
stream. If these streams have the same head (also unique) then they
must be the same sequence so the original blocks are
equivalent. Nifty.

Now this still isn't quite enough, we need one final data type:
`Supply`

``` haskell
    data Supply = Supply {-# UNPACK #-} !Int {-# UNPACK #-} !Int Block
        deriving (Eq,Ord,Show)

    blockSupply :: Block -> Supply
    blockSupply (Block i bs) = Supply i (i + blockSize - 1) (extract bs)
    {-# INLINE blockSupply #-}
```

A supply should be seen almost an iterator over a chunk of a number
line. We know that each block is 1024 away from each other and a
supply is almost an iterator from the blocks starting value over the
next 1023 elements. We know that `Supply`s could intersect because the
blocks are spaced this far apart.

Once we run out of those elements though, we need to get more. For
this we have another block hidden in the back of the supply. It's kept
lazily so that it won't fire of its first thunk to go bump our global
store. When we run out of things to enumerate we call `blockSupply`,
which will force `i` which will go bother the global counter for
another chunk of 1024 unique values.

With this understanding, `splitSupply` and `freshId` are quite easy.

``` haskell
    -- | An unboxed version of freshId
    freshId# :: Supply -> (# Int#, Supply #)
    freshId# (Supply i@(I# i#) j b)
      | i /= j = (# i#, Supply (i + 1) j b #)
      | otherwise = (# i#, blockSupply b #)
    {-# INLINE freshId# #-}

    -- | An unboxed version of splitSupply
    splitSupply# :: Supply -> (# Supply, Supply #)
    splitSupply# (Supply i k b) = case splitBlock# b of
        (# bl, br #)
          | k - i >= minSplitSupplySize
          , j <- i + div (k - i) 2 ->
            (# Supply i j bl, Supply (j + 1) k br #)
          | Block x (l :- r :- _) <- bl
          , y <- x + div blockSize 2
          , z <- x + blockSize - 1 ->
            (# Supply x (y - 1) l, Supply y z r #)
    {-# INLINE splitSupply# #-}
```

`freshId#` is more or less what we'd expect for an iterator. It
returns the lower bound and returns the new supply with the lower
bound bumped by one. Notice how cheap this is. In particular, since we
haven't forced `b` anywhere we've just copied a couple of words. The
expensive bit is when we actually run out of values in our range, in
this case we return our final value and force operation to produce a
new supply. This goes off and hammers on `blockCounter`. Happily we
only end up doing this 1/1024th of the time.

`splitSupply#` is a bit more complicated. When we go to split a supply
we're going to partition its range of values into two separate
ranges. However, we want to watch out for splitting extremely small
ranges. In this case, it's slightly more efficient to just bite the
bullet and incur the cost of hitting the `blockCounter`.

The way we determine this is to split the block `b`, giving us two new
blocks. If we have more in the current set of ids then
`minimumSplitSize` all we give the two blocks to two new supplies,
each with one half of the original range.

If the block size is indeed two small, we poke the first block in the
pair. This causes it to go hammer `blockCounter` and from there we
divide the range we got back into two and return these smaller
supplies over the new range. Notice that we've completely tossed the
remaining elements in the supply on the floor since there weren't that
many. More interestingly, we completely ignored the second result of
our split! The idea is that the most expensive operation we can do
here is force that first thunk in a block. However, is long as we
don't force their first components blocks are dirt cheap! Hence it's
cheaper to accept that we only get half of `blockSize` on each
`Supply` but we only had to perform one CAS to get them.

So now that we've done all of that, all that's left in the module is
the paper-thin wrappers over these functions so we don't always have
to use unboxed tuples

``` haskell
    -- | Obtain a fresh Id from a Supply.
    freshId :: Supply -> (Int, Supply)
    freshId s = case freshId# s of
      (# i, s' #) -> (I# i, s')
    {-# INLINE freshId #-}

    -- | Split a supply into two supplies that will return disjoint identifiers
    splitSupply :: Supply -> (Supply, Supply)
    splitSupply s = case splitSupply# s of
      (# l, r #) -> (l, r)
    {-# INLINE splitSupply #-}
```

And that's all. I'll hope this illustrated a fairly unique mix of
laziness in side effects to help reduce contention for a difficult
concurrent problem.

Cheers

[hackage-link]: http://hackage.haskell.org/package/concurrent-supply
