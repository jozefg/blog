----
title: Naive Map Isn't So Naive
tags: haskell
----

One of the most beloved functions in functional programming languages is `map`. It can be defined like this:

``` haskell
    map :: (a -> b) -> [a] -> [b]
    map f (x:xs) = f x : map f xs
    map _ []     = []
```

However in a lot of languages, writing `map` like this is a no-no. It's not tail recursive! For example in OCaml

``` ocaml
    # let rec my_map f = function
       | [] -> []
       | x :: xs -> f x :: my_map f xs

    # my_map ((+) 1) list_from_0_to_5000000
    ... Wait a bit ...
    Error: Stackoverflow
```

Urk! That's annoying. The problem is that we have to make a recursive function call that can't be compiled down to
a loop (tail recursion). Well, let's look at how `map` is defined in Haskell to avoid this problem

``` haskell
    -- In Base
    map :: (a -> b) -> [a] -> [b]
    map f (x:xs) = f x : map f xs
    map _ []     = []
```

Wait, isn't this bad? We just saw how this isn't tail recursive!

The thing is, in Haskell things are lazy. `map (+1) [1..10000]` returns a thunk. Inside that thunk is something like this

``` haskell
    (:) 1 {thunk to get rest of list}
```

So this takes constant space! After all, none of those extra stack frames are used because `:` doesn't evaluate its arguments.
This means that a lot of not tail recursive functions in Haskell still take constant space, however, you have to be careful
about consuming the results.

Take for example `sum`.

``` haskell
    sum :: [Integer] -> Integer
    sum (x:xs) = x + sum xs
    sum []     = 0
```

Now this also isn't tail recursive, but there's a problem: `+` is strict. By this I mean that to evaluate `a+b`
you must first evaluate `a` and then `b`. This means that to evaluate `x + sum xs` we have to evaluate `sum xs`.

Urk, now we're building up a big pile of expressions, something like

``` haskell
    a + sum (b:c:d:e:[])
    a + (b + sum (c:d:e:[]))
    a + (b + (c + sum (d:e:[])))
    a + (b + (c + (d + sum (e:[]))))
    a + (b + (c + (d + (e + 0))))
```

Now we can see why this will blow up, it's building up a huge expression
before we can evaluate anything. Hi stack overflow.

Now this is when we do want the tail recursive function like `foldl'` to keep
things in constant space.

### Conclusion

When you construct something with `:` for example, it's possible
to evaluate the head without evaluating the tail. Similarly with most
constructors. With things like this, it's possible to keep naive recursion
in constant space.
