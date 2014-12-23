---
title: Notes on Parametricity
tags: types, notes
---

I like types. If you haven't figured this out from my blog I really
don't know where you've been looking :) If you've ever talked to me in
real life about why I like types, chances are I mentioned ease of
reasoning and correctness.

Instead of showing how to prove parametricity I'd like to show how to
rigorously apply parametricity. So we'll be a step above handwaving
and a step below actually proving everything correct.

## What is Parametricity

At a high level parametricity is about the behavior of well typed
terms. It basically says that when we have more polymorphic types,
there are fewer programs that type check. For example, the type

``` haskell
    const :: a -> b -> a
```

Tells us everything we need to know about `const`. It returns it's
first argument. In fact, if it returns anything (non-bottom) at all,
it simply *must* be its first argument!

Parametricity isn't limited to simple cases like this however, it can
be used to prove that the type

``` haskell
    forall c. c -> (a -> c -> c) -> c
```

Is completely isomorphic to `[a]`!

We can use parametricity to prove free theorems, like if `map id = id`
then `map f . map g = map (f . g)`.

These are non-obvious properties and yet parametricity gives us the
power to prove all of them without even looking at the implementation
of these functions. That's pretty cool!

## Handwavy Parametricity

In order to get an idea of how to use parametricity, let's do some
handwavy proofs to get some intuition for how parametricity works.

Start with `id`.

``` haskell
    id :: a -> a
```

We know right away that `id` takes some value of type `a` and returns
another value `a`. Most people would safely guess that the returned
value is the one we fed it.

In fact, we can kinda see that this is the *only* thing it could
do. If it didn't, then somehow it'd have to create a value of type
`a`, but we know that that's impossible! (Yeah, yeah, I know,
bottom. Look the other way for now)

Similarly, if `map id` is just `id`, then we know that `map` isn't
randomly dropping some elements of our list. Since `map` isn't
removing elements, in order to take an `a` to a `b`, `map` has to be
applying `f` to each element! Since that's true, we can clearly see
that

``` haskell
    map f . map g = map (f . g)
```

because we know that applying `f` and then applying `g` is the same as
apply `f` and `g` at the same time!

Now these handwavy statements are all based on one critical point. No
matter how we instantiate a type variable, the behaviour we get is
related. Instantiating something to `Bool` or `Int` doesn't change the
fundamental behaviour about what we're instantiated.

## Background

Before we can formally define parametricity we need to flesh out a few
things. First things first, we need to actually specify the language
we're working in. For our purposes, we'll just deal with pure System
F.

    ty ::= v                [Type Variables]
         | ty -> ty         [Function Types]
         | forall v. ty     [Universal Quantification]
         | Bool             [Booleans]

    exp ::= v               [Variables]
          | exp exp         [Application]
          | λv : ty -> exp  [Abstraction]
          | Λv -> exp       [Type Abstraction]
          | exp[ty]         [Type Application]
          | true            [Boolean]
          | false           [Boolean]

The only real notable feature of our language is that all polymorphism
is explicit. In order to have a full polymorphic type we have to use a
"big lambda" Λ. This acts just like a normal lambda except instead of
abstracting over a term this abstracts over a type.

For example the full term for the identity function is

    id = Λ A -> \x : A -> x

From here we can explicitly specialize a polymorphic type with type
application.

    id[Bool] true

Aside from this, the typing rules for this language are pretty much
identical to Haskell's. In the interest of brevity I'll elide them.

## Actual Parametricity

Now that we have our language, let's talk about what we're interested
in proving. Our basic goal is to show that two expressions `e1` and
`e2` are equal. However, we don't want to use a `==` sort of
equality. We really mean that they can't be distinguished by our
programs. That for all programs with a "hole", filling that hole with
`e1` or `e2` will produce identical results. This is called
"observational equivalence" usually and notated with `≅`.

This is a bit more general than just `==`, for example it let's us say
that `flip const () ≅ id`. Now let's define another notion of
equality, logical equivalence.

This logical equivalence is an attempt to define equality without just
saying "running everything produces the same result". It turns out
it's really really hard to prove things that aren't syntactically
equivalent will always produce the same result!

Our logical equivalence `~` is defined in a context `η : δ ↔ δ'`. The
reason for this is that our terms may have free type variables and we
need to know how to deal with them. Each δ maps the free types in the
types of our terms to a concrete types and η is a relationship for
comparing `δ(v)` with `δ'(v)`.

Put less scarily, `η` is a set of rules that say how to compare two
terms when the have both are of type `v`. This is an important part of
our logical relation: it deals with open terms, terms with free
variables.

Now η isn't composed of just any relationship between terms, it has to
be "admissible". Admissibility means that for some relation R, two
conditions hold

 1. If `e R e'` and `d ⇒ e` and `d' ⇒ e'`, then `d R d'`
 2. If `e R e'` and `d ≅ e` and `d' ≅ e'`, then `d R d'`

The first rule means that `R` is closed under evaluation and the
second says that `R` respects observational equivalence.

Now we define our logical equivalence in some context δ to be

 1. When `e, e' : τ`, `e ~ e' [η]`
    if `e δ(t) e'`
 2. When `e, e' : Bool`, `e ~₂ e' [η]`
    if `e ⇓ v` and `e' ⇓ v`
 3. When `f, g : a → b`, `f ~ g [η]`
    if when `a ~ b [η]`, `f a ~ g b [η]`
 4. When `e e' : ∀ v. t`, `e ~ e' [η]`  \
    if `R : p ↔ p'`, `e[p] ~ e'[p'] [η[v ↦ R]]`

Now this rule has 4 cases, one for each type. That's the first
critical bit of this relation, we're talking about things by the
structure of the type, not the value itself.

Now with this in mind we can state the full parametricity theorem.

> For all expressions e and mappings η, `e ~ e [η]`

That's it! Now this is only really useful when we're talking about
polymorphic type, then parametricity states that for any admissible
relation `R`, two different instantiations are related.

While I won't go into how to prove it, another important results we'll
use for proofs with parametricity is that `(∀η. e ~ e' [η]) ⇔ e ≅ e'`.

## Applying Parametricity

Now that I've said exactly what parametricity is, I'd like to step
through a few proofs. The goal here is to illustrate how we can use
this to prove some interesting properties.

First we just have to prove the classic result that any
`f : forall a. a -> a` is equivalent to `id = Λa. λx : a. x`.

To prove this we need to show `f ~ id [η]`. For this we need to show
that for any admissible relation `R` between `τ` and `τ'`, then
`f[τ] ~ λx : τ'. x [η[a ↦ R]`. Stepping this one more time we end up
with the goal that `e R e'` then `f[τ] e ~ e' ⇔ f[τ] e R e'`

Now this is where things get tricky and where we can apply
parametricity. We know by definition that `f ~ f [η]`. We then choose
a new relation `S : τ' ↔ τ'` where `d S d'` if and only `d ≅ e'` and
`d' ≅ e'`. Exercise to the reader: show admissibility.

From here we know that `f[τ] ~ f[τ] [η[a ↦ R]]` and since `e S e`
then `f[τ] e ~ f[τ] e` which implies `f[τ] e S f[τ] e`. This means
that `f[τ] e ≅ e`.  From our note above, `f[τ] e ~ e` and by
transitivity we have `f[τ] e R e'`.

Now we can prove something similar, that
`(f : a → b → a) ≅ const`. The proof is very similar,

     f ~ const [η]
     f[τ][ν] ~ const[τ'][ν'] [η[a ↦ R][b ↦ S]]
     f[τ][ν] a b ~ a' [η[a ↦ R][b ↦ S]] where a R a'

Now we need to show that `f a b ≅ a`. For this we define `T` to be an
admissible relationship where `d T d'` if and only if
`d ≅ a ≅ d'`. From here we also define `U` to be an admissible
relation where `a U b` if and only if `a ~ b`.

Now we know that `f ~ f [η]` and so

    f[τ][ν] ~ f[τ'][ν'] [η[a ↦ T][b ↦ U]]`

And since `a T a` and `b U b`, we know that

    f[τ][ν] a b ~ f[τ'][ν'] a b [η[a ↦ T][b ↦ U]]

this means that `f a b ≅ a` and completes our proofs. Hopefully this
reinforces the idea of using parametricity and admissible
relationships to produces our properties.

Now for something a bit trickier. Church numerals are a classic idea
from lambda calculus where

     0 ≡ λs. λz. z
     1 ≡ λs. λz. s z
     2 ≡ λs. λz. s (s z)

And so on. In terms of types,

``` haskell
    type Nat = forall c. (c → c) → c → c
```

Now intuitively from this type it seems obvious that this only allows
us to apply the first argument `n` types to the second, like a church
numeral. Because of this we want to claim that we can compose the
first argument with itself `n` times before applying it to the second
or for all `c : Nat`, there exists an `n` so that `compose n ≡ c`.

To prove this we proceed as before and we end up with


     compose[τ] s z ~ c[τ'] s' z' [η[c ↦ R]]

Now we define a new relation `S` where

  1. `a S b` if `a ≅ z' ≅ b`
  2. `a S b` if `n S n'` and `a ≅ s' n` and `b ≅ s' n'`

Now we know that `c[τ'] s' z' S c[τ'] s' z'` so by inversion on this
we can determine that `n` applications of `s'` followed by `z'`.

Set the `n` for compose to this new `n`. From here our result follows
by induction on `n`.

This proof means there's a mapping from `c` to `n`. The curious reader
is encouraged to show this is an invertible mapping and complete the
proof of isomorphism.

## A Note on Free Theorems

Now most people in the Haskell community have heard the term "free
theorem", what they may not realize is that free theorems are a direct
result of parametricity.

In fact, if you read Wadler's original paper sections 5 and onwards
establish parametricity. What's interesting here is that Wadler opts
to establish it in a similar way to how Reynolds did. He first defines
a mathematical structure called a "type frame".

This structure lets us map a program in something like System F or
Haskell into pure math functions. From there it defines relationships
in a similar way to our logical relation and shows it's reflexive.

I didn't opt for this route because

 1. Denotational semantics scare me a bit
 2. Type frames need more math to make sense

It's still definitely worth [reading][wadler] for the curious though.

## Wrap Up

Now that we've defined parametricity and established a few theorems
for it, I hope you can start to see the advantage of types to guide
our programs. General enough types can give us assurances without
every even looking at the code in question.

Aren't types cool?

[wadler]: http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.38.9875
