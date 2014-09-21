---
title: Introduction to Dependent Types: Off, Off to Agda Land
---

*First, an apology. Sorry this has take so long to push out. I've just
started my first semester at Carnegie Mellon. I fully intend to keep
blogging, but it's taken a little while to get my feet under me. Happy
readings :)*


In this second post of my "intro to dependent types" series we're
going on a whirlwind tour of Agda. Specifically we're going to look at
translating our faux-Haskell from the last post into honest to
goodness typecheckable Agda.

There are 2 main reasons to go through the extra work of using a real
language rather than pseudo-code

 1. This is typecheckable.
    I can make sure that all the i's are dotted and t's crossed.
 2. It's a lot cleaner
    We're only using the core of Agda so it's more or less a very
    stripped down Haskell with a much more expressive but simpler type
    system.

With that in mind let's dive in!

### What's the Same

There's quite a bit of shared syntax between Agda and Haskell, so a
Haskeller can usually guess what's going on.

In Agda we still give definitions in much the same way (single `:`
though)

``` agda
    thingy : Bool
    thingy = true
```

where as in Haskell we'd say

``` haskell
    name :: Type
    name = val
```

In fact, we even get Haskell's nice syntactic sugar for functions.

``` agda
    function : A -> B -> ... -> C
    function a b ... = c
```
Will desugar to a lambda.

``` agda
    function : A -> B -> ... -> C
    function = \a b ... -> c
```

One big difference between Haskell and Agda is that, due to Agda's
more expressive type system, type inference is woefully
undecidable. Those top level signatures are not optional sadly. Some
DT language work a little harder than Agda when it comes to inference,
but for a beginner this is a bit of a feature: you learn what the
actual (somewhat scary) types are.

And of course, you always give type signatures in Haskell I'm sure :)

Like Haskell function application is whitespace and functions are
curried

``` agda
    -- We could explicitly add parens
    -- foo : A -> (B -> C)
    foo : A -> B -> C
    foo = ...

    a : A
    a = ...

    bar : B -> C
    bar = foo a
```

Even the data type declarations should look familiar, they're just
like GADTs syntactically.

``` agda
    data Bool : Set where
      true  : Bool
      false : Bool
```

Notice that we have this new `Set` thing lurking in our code. `Set` is
just the kind of normal types, like `*` in Haskell. In Agda there's
actually an infinite tower of these `Bool : Set : Set1 : Set2 ...`,
but won't concern ourselves with anything beyond `Set`. It's also
worth noting that Agda doesn't require any particular casing for
constructors, traditionally they're lower case.

Pattern matching in Agda is pretty much identical to Haskell. We can
define something like

``` agda
    not : Bool -> Bool
    not true  = false
    not false = true
```

One big difference between Haskell and Agda is that pattern matching
**must** be exhaustive. Nonexhaustiveness is a compiler error in Agda.

This brings me to another point worth mentioning. Remember that
structural induction I mentioned the other day? Agda only allows
recursion when the terms we recurse on are "smaller".

In other words, all Agda functions are defined by structural
induction. This together with the exhaustiveness restriction means
that Agda programs are "total". In other words all Agda programs
reduce to a single value, they never crash or loop forever.

This can occasionally cause pain though since not all recursive
functions are modelled nicely by structural induction! A classic
example is merge sort. The issue is that in merge sort we want to say
something like

``` agda
    mergeSort : List Nat -> List Nat
    mergeSort [] = []
    mergeSort (x :: []) = x :: []
    mergeSort xs = let (l, r) = split xs in
                     merge (mergeSort l, mergeSort r)
```

But wait, how would the typechecker know that `l` and `r` are strictly
smaller than `xs`? In fact, they might not be! We know that the length
of `length xs > 1`, but convincing the typechecker of that fact is a
pain! In fact, without elaborate trickery, Agda will reject this
definition.

So, apart from these restriction for totality Agda has pretty much
been a stripped down Haskell. Let's start seeing what Agda offers over
Haskell.

### Dependent Types

There wouldn't be much point in writing Agda if it didn't have
dependent types. In fact the two mechanisms that comprise our
dependent types translate wonderfully into Agda.

First we had pi types, remember those?

``` haskell
    foo :: (a :: A) -> B
    foo a = ...
```

Those translate almost precisely into Agda, where we'd write

``` agda
    foo : (a : A) -> B
```

The only difference is the colons! In fact, Agda's pi types are far
more general than what we'd discussed previously. The extra generality
comes from what we allow `A` to be. In our previous post, `A` was
always some normal type with the kind `*` (`Set` in Agda). In Agda
though, we allow `A` to be `Set` itself. In Haskell syntax that would
be something like

``` haskell
    foo :: (a :: *) -> B
```

What could `a` be then? Well anything with the kind `*` is a type,
like `Bool`, `()`, or `Nat`. So that `a` is like a normal type
variable in Haskell

``` haskell
    foo :: forall a. B
```

In fact, when we generalize pi types like this, they generalize
parametric polymorphism. This is kind of like how we use "big lambdas"
in System F to write out polymorphism explicitly.

Here's a definition for the identity function in Agda.

``` agda
    id : (A : Set) -> A -> A
    id A a = a
```

This is how we actually do all parametric polymorphism in Agda, as a
specific use of pi types. This comes from the idea that types are also
"first class". We can pass them around and use them as arguments to
functions, even dependent arguments :)

Now our other dependently typed mechanism was our generalized
generalized algebraic data types. These also translate nicely to Agda.

``` agda
    data Foo : Bool -> Set where
      Bar : Foo True
```

We indicate that we're going to index our data on something the same
way we would in Haskell++, by adding it to the type signature on the
top of the data declaration.

Agda's GGADTs also allow us to us to add "parameters" instead of
indices. These are things which the data type may use, but each
constructor handles uniformly without inspecting it.

For example a list type depends on the type of it's elements, but it
doesn't poke further at the type or value of those elements. They're
handled "parametrically".

In Agda a list would be defined as

``` agda
    data List (A : Set) : Set where
      nil  : List A
      cons : A -> List A -> List A
```

If your wondering what on earth the difference is, don't worry! You've
already in fact used parametric/non-parametric type arguments in
Haskell. In Haskell a normal algebraic type can just take several type
variables and can't try to do clever things depending on what the
argument is. For example, our definition of lists

``` haskell
    data List a = Cons a (List a) | Nil
```

can't do something different if `a` is `Int` instead of `Bool` or
something like that. That's not the case with GADTs though, there we
can do clever things like

``` haskell
    data List :: * -> * where
      IntOnlyCons :: Int -> List Int -> List Int
      ...
```

Now we're not treating our type argument opaquely, we can figure
things out about it depending on what constructor our value uses!
That's the core of the difference between parameters in indices in
Agda.

Next let's talk about modules. Agda's prelude is absolutely tiny. By
tiny I mean essentially non-existant. Because of this I'm using the
Agda standard library heavily and to import something in Agda we'd
write

    import Foo.Bar.Baz

This isn't the same as a Haskell import though. By default, imports in
Agda import a qualified name to use. To get a Haskell style import
we'll use the special shortcut

    open import Foo.Bar

which is short for

    import Foo.Bar
    open Bar

Because Agda's prelude is so tiny we'll have to import things like
booleans, numbers, and unit. These are all things defined in the
standard library, not even the core language. Expect any Agda code
we write to make heavy use of the standard library and begin with a
lot of imports.

Finally, Agda's names are somewhat.. unique. Agda and it's standard
library are unicode heavy, meaning that instead of unit we'd type ⊤
and instead of `Void` we'd use ⊥. Which is pretty nifty, but it does
take some getting used to. If you're familiar with LaTeX, the Emacs
mode for Agda allows LaTeX style entry. For example ⊥ can be entered
as `\bot`.

The most common unicode name we'll use is ℕ. This is just the type of
natural numbers as their defined in `Data.Nat`.

### A Few Examples

Now that we've seen what dependent types look like in Agda, let's go
over a few examples of their use.

First let's import a few things

``` agda
    open import Data.Nat
    open import Data.Bool
```

Now we can define a few simple Agda functions just to get a feel for
how that looks.

``` agda
    not : Bool -> Bool
    not true  = false
    not false = true

    and : Bool -> Bool -> Bool
    and true b  = b
    and false _ = false

    or : Bool -> Bool -> Bool
    or false b = b
    or true  _ = true
```

As you can see defining functions is mostly identical to Haskell, we
just pattern match and the top level and go from there.

We can define recursive functions just like in Haskell

``` adga
    plus : ℕ -> ℕ -> ℕ
    plus (suc n) m = suc (plus n m)
    plus zero    m = m
```

Now with Agda we can use our data types to encode "proofs" of sorts.

For example

``` agda
    data IsEven : ℕ -> Set where
      even-z : IsEven zero
      even-s  : (n : Nat) -> IsEven n -> IsEven (suc (suc n))
```

Now this inductively defines what it means for a natural number to be
even so that if `Even n` exists then `n` must be even. We can also
state oddness

``` agda
    data IsOdd : ℕ -> Set where
      odd-o : IsOdd (suc zero)
      odd-s : (n : ℕ) -> IsOdd n -> IsOdd (suc (suc n))
```

Now we can construct a decision procedure which produces either a
proof of evenness or oddness for all natural numbers.

``` agda
    open import Data.Sum -- The same thing as Either in Haskell; ⊎ is just Either

    evenOrOdd : (n : ℕ) -> Odd n ⊎ Even n
```

So we're setting out to construct a function that, given any `n`,
builds up an appropriate term showing it is either even or odd.

The first two cases of this function are kinda the base cases of this
recurrence.

``` agda
    evenOrOdd zero = inj₁ even-z
    evenOrOdd (suc zero) = inj₂ odd-o
```

So if we're given zero or one, return the base case of `IsEven` or
`IsOdd` as appropriate. Notice that instead of `Left` or `Right` as
constructors we have `inj₁` and `inj₂`. They serve exactly the same
purpose, just with a shinier unicode name.

Now our next step would be to handle the case where we have

``` agda
    evenOrOdd (suc (suc n)) = ?
```

Our code is going to be like the Haskell code

``` haskell
    case evenOrOdd n of
      Left evenProof -> Left (EvenS evenProof)
      Right oddProof -> Right (OddS  oddProof)
```

In words, we'll recurse and inspect the result, if we get an even
proof we'll build a bigger even proof and if we can an odd proof we'll
build a bigger odd proof.

In Agda we'll use the `with` keyword. This allows us to "extend" the
current pattern matching by adding an expression to the list of
expressions we're pattern matching on.

``` agda
    evenOrOdd (suc (suc n)) with evenOrOdd n
    evenOrOdd (suc (suc n)) | inj₁ x = ?
    evenOrOdd (suc (suc n)) | inj₂ y = ?
```

Now we add our new expression to use for matching by saying
`... with evenOrOdd n`. Then we list out the next set of possible
patterns.

From here the rest of the function is quite straightforward.

``` agda
    evenOrOdd (suc (suc n)) | inj₁ x = inj₁ (even-s n x)
    evenOrOdd (suc (suc n)) | inj₂ y = inj₂ (odd-s n y)
```

Notice that we had to duplicate the whole `evenOrOdd (suc (suc n))`
bit of the match? It's a bit tedious so Agda provides some sugar. If
we replace that portion of the match with `...` Agda will just
automatically reuse the pattern we had when we wrote `with`.

Now our whole function looks like

``` agda
    evenOrOdd : (n : ℕ) -> IsEven n ⊎ IsOdd n
    evenOrOdd zero = inj₁ even-z
    evenOrOdd (suc zero) = inj₂ odd-o
    evenOrOdd (suc (suc n)) with evenOrOdd n
    ... | inj₁ x = inj₁ (even-s n x)
    ... | inj₂ y = inj₂ (odd-s n y)
```

How can we improve this? Well notice that that `suc (suc n)` case
involved unpacking our `Either` and than immediately repacking it,
this looks like something we can abstract over.

``` agda
    bimap : (A B C D : Set) -> (A -> A) -> (B -> B) -> A ⊎ B -> A ⊎ B
    bimap A B C D f g (inj₁ x) = inj₁ (f x)
    bimap A B C D f g (inj₂ y) = inj₂ (g y)
```

If we gave `bimap` a more Haskellish siganture

``` haskell
    bimap :: forall a b c d. (a -> c) -> (b -> d) -> Either a b -> Either c d
```

One interesting point to notice is that the *type* arguments in the
Agda function (`A` and `B`) also appeared in the normal argument
pattern! This is because we're using the normal pi type mechanism for
parametric polymorphism, so we'll actually end up explicitly passing
and receiving the types we quantify over. This messed with me quite a
bit when I first starting learning DT languages, take a moment and
convince yourself that this makes sense.

Now that we have `bimap`, we can use it to simplify our `evenOrOdd`
function.

``` agda
    evenOrOdd : (n : ℕ) -> IsEven n ⊎ IsOdd n
    evenOrOdd zero = inj₁ even-z
    evenOrOdd (suc zero) = inj₂ odd-o
    evenOrOdd (suc (suc n)) =
      bimap (IsEven n) (IsOdd n)
            (IsEven (suc (suc n))) (IsOdd (suc (suc n)))
            (even-s n) (odd-s n) (evenOrOdd n)
```

We've gotten rid of the explicit `with`, but at the cost of all those
explicit type arguments! Those are both gross and obvious. Agda can
clearly deduce what `A`, `B`, `C` and `D` should be from the arguments
and what the return type must be. In fact, Agda provides a convenient
mechanism for avoiding this boilerplate. If we simply insert `_` in
place of an argument, Agda will try to guess it from the information
it has about the other arguments and contexts. Since these type
arguments are so clear from context, Agda can guess them all

``` agda
    evenOrOdd : (n : ℕ) -> IsEven n ⊎ IsOdd n
    evenOrOdd zero = inj₁ even-z
    evenOrOdd (suc zero) = inj₂ odd-o
    evenOrOdd (suc (suc n)) =
      bimap _ _ _ _ (even-s n) (odd-s n) (evenOrOdd n)
```

Now at least the code fits on one line! This also raises something
interesting, the types are so strict that Agda can actually figure out
parts of our programs for us! I'm not sure about you but at this point
in time my brain mostly melted :) Because of this I'll try to avoid
using `_` and other mechanisms for Agda writing programs for us where
I can. The exception of course being situations like the above where
it's necessary for readabilities sake.

One important exception to that rule is for parameteric
polymorphism. It's a royal pain to pass around types explicitly
everywhere. We're going to use an Agda feature called "implicit
arguments". You should think of these as arguments for which the `_`
is inserted for it. So instead of writing

``` agda
    foo _ zero zero
```

We could write

``` agda
    foo zero zero
```

This more closely mimicks what Haskell does for its parametric
polymorphism. To indicate we want something to be an implicit
argument, we just wrap it in `{}` instead of `()`. So for example, we
could rewrite `bimap` as

``` agda
    bimap : {A B C D : Set} -> (A -> A) -> (B -> B) -> A ⊎ B -> A ⊎ B
    bimap f g (inj₁ x) = inj₁ (f x)
    bimap f g (inj₂ y) = inj₂ (g y)
```
To avoid all those underscores.

Another simple function we'll write is that if we can construct an
`IsOdd n`, we can build an `IsEven (suc n)`.

``` agda
    oddSuc : (n : ℕ) -> IsOdd n -> IsEven (suc n)
```

Now this function has two arguments, a number and a term showing that
that number is odd. To write this function we'll actually recurse on
the `IsOdd` term.

``` agda
    oddSuc .1 odd-o = even-s zero even-z
    oddSuc .(suc (suc n)) (odd-s n p) = even-s (suc n) (oddSuc n p)
```

Now if we squint hard and ignore those `.` terms, this looks much like
we'd expect. We build the `Even` starting from `even-s zero
even-z`. From there we just recurse and talk on a `even-s` constructor
to scale the `IsEven` term up by two.

There's a weird thing going on here though, those `.` patterns. Those
are a nifty little idea in Agda that pattern matching on one thing
might *force* another term to be some value. If we know that our
`IsOdd n` is `odd-o` `n` **must** be `suc zero`. Anything else would
just be completely incorrect. To notate these patterns Agda forces you
to prefix them with `.`. You should read `.Y` as "because of X, this
*must* be Y".

This isn't an optional choice though, as `.` patterns may do several
wonky things. The most notable is that they often use pattern
variables nonlinearly, notice that `n` appeared twice in our second
pattern clause. Without the `.` this would be very illegal.

As an exercise to the reader, try to write

``` agda
    evenSuc : (n : ℕ) -> IsEven n -> IsOdd (suc n)
```

### Wrap Up

That wraps up this post which came out much longer than I
expected. We've now covered enough basics to actually discuss
meaningful dependently typed programs. That's right, we can finally
kiss natural numbers good bye in the next post!

Next time we'll cover writing a small program but interesting program
and use dependent types to assure ourselves of it's correctness.

As always, please comment with any questions :)
