---
title: A Crash Course on ML Modules
tags: sml, haskell
---

I was having lunch with a couple of Haskell programmers the other day
and the subject of the ML family came up. I've been writing a lot of
ML lately and mentioned that I thought *ML was well worth learning for
the average Haskeller. When pressed why the best answer I could come
up with was "Well.. clean language, Oh! And an awesome module system"
which wasn't my exactly most compelling response.

I'd like to outline a bit of SML module system here to help
substantiate why looking at an ML is A Good Thing. All the code here
should be translatable to OCaml if that's more your taste.

## Concepts

In ML languages modules are a well thought out portion of the
language. They aren't just "Oh we need to separate these
names... modules should work". Like any good language they have
methods for abstraction and composition. Additionally, like any good
part of an ML language, modules have an expressive type language for
mediating how composition and abstraction works.

So to explain how this module system functions as a whole, we'll cover
3 subjects

 1. Structures
 2. Signatures
 3. Functors

Giving a cursory overview of what each thing is and how it might be
used.

## Structures

Structures are the values in the module language. They are how we
actually create a module. The syntax for them is

``` sml
    struct
      fun flip f x y = f y x
      datatype 'a list = Con of ('a * 'a list) | Nil
      ...
    end
```

*A quick note to Haskellers, in ML types are lower case and type
 variables are written with 's. Type constructors are applied
 "backwards" so `List a` is `'a list`.*

So they're just a bunch of a declarations stuffed in between a
`struct` and `end`. This is a bit useless if we can't bind it to a
name. For that there's

``` sml
    structure M = struct val x = 1 end
```

And now we have a new module `M` with a single member, `x : int`. This
is just like binding a variable in the term language except a "level
up" if you like. We can use this just like you would use modules in
any other language.

``` sml
    val x' = M.x + 1
```

Since `struct ... end` can contain any list of declarations we can
nest module bindings.

``` sml
    structure M' =
      struct
        structure NestedM = M
      end
```

And access this using `.`.

``` sml
    val sum = M'.NestedM.x + M.x
```

As you can imagine, it would get a bit tedious if we needed to `.` our
way to every single module access. For that we have `open` which just
dumps a module's exposed contents into our namespace. What's
particularly interesting about `open` is that it is a "normal"
declaration and can be nested with `let`.

``` sml
    fun f y =
      let open M in
        x + y
      end
```

OCaml has gone a step further and added special syntax for small
opens. The "local opens" would turn our code into

``` ocaml
    let f y = M.(x + y)
```

This already gives us a lot more power than your average module
system. Structures basically encapsulate what we'd expect in a module
system, but

 1. Structures =/= files
 2. Structures can be bound to names
 3. Structures can be nested

Up next is a look at what sort of type system we can impose on our
language of structures.

## Signatures

Now for the same reason we love types in the term language (safety,
readability, insert-semireligious-rant) we'd like them in the module
language. Happily ML comes equipped with a feature called
signatures. Signature values look a lot like structures

``` sml
    sig
      val x : int
      datatype 'a list = Cons of ('a * 'a list) | Nil
    end
```

So a signature is a list of declarations *without* any
implementations. We can list algebraic data types, other modules, and
even functions and values but we won't provide any actual code to run
them. I like to think of signatures as what most documentation
rendering tools show for a module.

As we had with structures, signatures can be given names.

``` sml
    signature MSIG = sig val x : int end
```

On their own signatures are quite useless, the whole point is that we
can apply them to modules after all! To do this we use `:` just like
in the term language.

``` sml
    structure M : MSIG = struct val x = 1 end
```

When compiled, this will check that `M` has at least the field
`x : int` inside its structure. We can apply signatures retroactively
to both module variables and structure values themselves.

``` sml
    structure M : MSIG = struct val x = 1 end : MSIG
```

One interesting feature of signatures is the ability to leave certain
types abstract. For example, when implementing a map the actual
implementation of the core data type doesn't belong in the signature.

``` sml
    signature MAP =
      sig
        type key
        type 'a table

        val empty : 'a table
        val insert : key -> 'a -> 'a table -> 'a table
        val lookup : key -> 'a table -> 'a option
      end
```

Notice that the type of keys and tables are left abstract. When
someone applies a signature they can do so in two ways, weak or
strong ascription. Weak ascription (`:`) means that the constructors
of abstract types are still accessible, but the signature *does* hide
all unrelated declarations in the module. Strong ascription (`:>`)
makes the abstract types actually abstract.

Every once in a while we need to modify a signature. We can do this
with the keywords `where type`. For example, we might implement a
specialization of `MAP` for integer keys and want our signature to
express this

``` sml
    structure IntMap :> MAP where type key = int =
      struct ... end
```

This incantation leaves the type of the table abstract but specializes
the keys to an int.

Last but not least, let's talk about abstraction in module land.

## Functors

Last but not least let's talk about the "killer feature" of ML module
systems: functors. Functors are the "lifting" of functions into the
module language. A functor is a function that maps modules with a
certain signature to functions of a different signature.

Jumping back to our earlier example of maps, the equivalent in Haskell
land is `Data.Map`. The big difference is that Haskell gives us maps
for all keys that implement `Ord`. Our signature doesn't give us a
clear way to associate all these different modules, one for each
`Ord`erable key, that are really the same thing. We can represent this
relationship in SML with

``` sml
    signature ORD =
      sig
        type t
        val compare : t * t -> order
      end

    functor RBTree (O : ORD) : MAP where type key = O.t =
      struct
        open O
        ....
      end
```

Which reads as "For any module implementing `Ord`, I can give you a
module implementing `MAP` which keys of type `O.t`". We can then
instantiate these

``` sml
    structure IntOrd =
      struct
        type t = int
        val compare = Int.compare end
      end
    structure IntMap = RBTree(IntOrd)
```

Sadly SML's module language isn't higher order. This means we can't
assign functors a type (there isn't an equivalent of `->`) and we
can't pass functors to functors. Even with this restriction functors
are tremendously useful.

One interesting difference between SML and OCaml is how functors
handle abstract types. Specifically, is it the case that

    F(M).t = F(M).t

In SML the answer is (surprisingly) no! Applying a functor generates
brand new abstract types. This is actually beneficial when you
remember SML and OCaml aren't pure. For example you might write a
functor for handling symbol tables and internally use a mutable symbol
table. One nifty trick would be to keep of type of symbols
abstract. If you only give back a symbol upon registering something in
the table, this would mean that all symbols a user can supply are
guaranteed to correspond to some entry.

This falls apart however if functors are extensional. Consider the
following REPL session

``` sml
    > structure S1 = SymbolTable(WhateverParameters)
    > structure S2 = SymbolTable(WhateverParameters)
    > val key = S1.register "I'm an entry"
    > S2.lookup key
    Error: no such key!
```

This will not work if `S1` and `S2` have separate key types.

To my knowledge, the general conclusion is that generative functors
(ala SML) are good for impure code, but applicative functors (ala
OCaml and BackPack) really shine with pure code.

## Wrap Up

We've covered a lot of ground in this post. This wasn't an exhaustive
tour of every feature of ML module systems, but hopefully I got the
jist across.

If there's one point to take home: In a lot of languages modules are
clearly a bolted on construction. They're something added on later to
fix "that library problem" and generally consist of the same "module
<-> file" and "A module imports others to bring them into scope". In
ML that's simply not the case. The module language is a rich, well
thought out thing with it's own methods of abstraction, composition,
and even a notion of types!

I wholeheartedly recommend messing around a bit with OCaml or SML to
see how having these things impacts your thought process. I think
you'll be pleasantly surprised.
