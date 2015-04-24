---
title: SML for Haskellers
tags: sml, haskell
---

Inspired by ezyang's [OCaml for Haskellers][ocaml-for-haskellers] I
decided to write something similar for SML. If you already know OCaml
I also recommend [Adam Chlipala's guide][adams-guide]

I'll follow mostly the same structure as Edward's article so we'll
have

    {- Haskell -}
    (* SML *)

## What Do They Have in Common

SML and Haskell have quite a lot in common

Common types:

    ()   | Int | Integer    | Char | Bool | String | Double | (A,  B,  C)
    unit | int | IntInf.int | char | bool | string | real   | (A * B * C)

Literals:

    () | 1 | 'a'  | True | "hello" | 3.14 | (1, 2, 3)
    () | 1 | #'a' | true | "hello" | 3.14 | (1, 2, 3)

Common operators

    == | /= | not | &&      | ||     | ++ | !!
    =  | <> | not | andalso | orelse | ^  | String.sub

Type variables:

    a  b
    'a 'AnythingGoes

Function application:

    f x y z
    f x y z

Lambdas:

    \x -> ...
    fn x => ...

If:

    if True then 1 else 0
    if true then 1 else 0

Pattern matching

    case x of
      Nothing -> ...
      Just a -> ...

    case x of
       NONE => ...
     | SOME a => ...

Top level functions support pattern matching in both:

    factorial 0 = 1
    factorial n = n * factorial (n - 1)

    fun factorial 0 = 1
      | factorial n = n * factorial (n - 1)

Top level bindings can be declared without the sugar for currying as well.

    f = \x -> \y -> x
    val f = fn x => fn y => x

We can have top level patterns in both as well:

     (a, b) = (1, 2)
     val (a, b) = (1, 2)

Type synonyms:

    type Three a b = (a, a, b)
    type ('a, 'b) three = 'a * 'a * 'b

Data types:

    data List a = Cons a (List a) | Nil
    datatype 'a list = Cons of 'a * 'a list | Nil

Notice that in ML data type constructors can only take on argument. This means
they often end up taking a tuple (or record). They are however normal functions
unlike in OCaml.

Type annotations:

    f :: a -> a
    f x = x

    fun f (x : 'a) : 'a = x

Type annotations for expressions:

    (1 + 1 :: Int)
    (1 + 1 :  int)

Let bindings:

    let x = 1     in x + x
    let val x = 1 in x + x end

Declare a new mutable reference:

    newIORef True
    ref true

Modify a mutable reference:

    setIORef r False
    r := false

Read a mutable reference:

    readIORef r
    ! r

Making exceptions:

    data MyExn = Exn String; instance Exception ... where
    exception Exn of string

Raising an exception:

    throw (Exn "uh oh")
    raise Exn "uh oh"

Catching an exception:

    catch e $ \(Exn s) -> s
    e handle Exn s => s

Since SML isn't a purely functional language, none of the last couple of
constructs listed live in anything monadic like their Haskell siblings. The type
of `r := false` is just `unit`, not `IO ()` or something.

## What Is SML Missing

Aside from the obvious things, like SML being strict so it's missing pervasive
lazy evaluation, SML is missing some things from Haskell.

the biggest gap I stumble across in SML is the lack of higher kinded
polymorphism:

    data Fix f = Fix (f (Fix f))
    datatype 'f fix = Fix of ('f fix) 'f (* Urk, syntax error *)

Even applying a type variable is a syntax error! As this might suggest to you,
SML's type system is much simpler than what we have in Haskell. It doesn't have
a notion of type families, GADTs, fancy kinds, data type promotion, etc,
etc. SML is really limited to the areas of the Haskell type system you'd be
accustomed to after reading Learn You A Haskell! Just algebraic data types,
functions, and polymorphism.

Aside from this, SML doesn't have guards, nor a lot of syntactic sugar that
Haskell has. A nice exception to this is lambda cases, which is written

    fn 0 => 1
     | 1 => 2
     | n => 0

Additionally, SML doesn't have significant indentation which means that
occasionally awkward parenthesis is necessary. For example

    case x of
       true  => (case !r of
                  x => x + 1)
     | false => (r := 1; 2)

The parenthesis are mandatory.

On the stranger side, SML has records (discussed later) but they don't have a
functional updating operation. This is a pain to be honest. Also related, SML
has a somewhat nasty habit of allowing for ad-hoc overloading in the way most
languages do: certain expressions are "blessed" with unutterable types that must
be inferred from context. There are only a few of these, `+`, `*`, and record
accessors being among them. I'm personally not a huge fan, but in practice this
is almost never an issue.

Finally ML doesn't have Haskell-style type classes. I don't miss them, some
people would.

## What Is Haskell Missing (in Comparison)

Aside from the obvious things, like Haskell being lazy so it's missing pervasive
eager evaluation, SML does have a couple of interesting things.

Of course SML has actual modules. I've
[explained a bit about them earlier][modules]. This alone is reason enough to
write some ML. Additionally, SML has a saner notion of records. Records are a
type in and of themselves. This means we can have something like

``` sml
    type coord = {x : int, y : int}
```

However, since this is just a type synonym we don't actually need to declare
it. Accessors are written `#x` to access the field `x` from a record. SML
doesn't have a very advanced record system so `#x` isn't typeable. It's
overloaded to access a field from some record and the concrete record must be
inferrable from context. This often means that while we *can* have free floating
records, the inference woes make us want to wrap them in constructors like so

    data coord = Coord of {x : int, y : int}

This has the nice upshot that record accessors aren't horrible broken with
multiple constructors. Let's say we had

    datatype user = Person {firstName : string, lastName : string}
                  | Robot  {owner : string, guid : int}

We can't apply `#firstName` to an expression of type `user`. It's ill-typed
since `user` isn't a record, it has a constructor which *contains a
record*. In order to apply `#firstName` we have to pattern match first.

Finally, SML has a real, honest to goodness specification. In fact, SML is so
well specified it's been [completely mechanized][sml-in-twelf]. There is an
actual mechanized proof that SML is typesafe. The practical up shot of this is
that SML is rock solid. There's a definitive right answer to what a program
should do and that answer is "whatever that one true implementation does". In
fact, there are actually a lot of SML compilers and they're all reasonably
compliant. Two noteworthy ones

 1. SML/NJ - An interactive system for SML. This provides a REPL and is what we
    use at CMU for our introduction to functional programming courses.
 2. Mlton - A whole program optimizing compiler. Mlton produces stupidly fast
    code but is significantly slower for compilation.

Since SML is fully standardized, I general develop with NJ and eventually feed
the program into mlton if I intend the thing to run fast.

Also, modules are amazing, have I mentioned modules yet?

## Wrap Up

So now that we've gone through most of the basic syntactic constructs of SML,
most ML code should be readable. This is great because there's some interesting
pieces of ML code to read. In particular, these wonderful books are written with
ML

 - Purely Functional Data Structures
 - Compiling With Continuations
 - Modern Compiler Construction in ML

I recommend all three of these books heartily. If you're looking to learn about
compilers, the last one in particular is the best introduction I'm aware of. The
second one is an in depth look at a trick for compiling strict functional
language.

Other general books on ML if you decide you want to give SML a more serious look

 - ML for the Working Programmer
 - Elements of ML Programming
 - Programming in Standard ML

The course I'm TAing currently is based around the last one and it's freely
available online which is nice.

Cheers,

[ocaml-for-haskellers]: http://blog.ezyang.com/2010/10/ocaml-for-haskellers/
[adams-guide]: http://adam.chlipala.net/mlcomp/
[modules]: /posts/2015-01-08-modules.html
[sml-in-twelf]: https://github.com/SMLFamily/The-Mechanization-of-Standard-ML
