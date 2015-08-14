---
title: Solving Recursive Equations
tags: types
---

I wanted to write about something related to all the stuff I've been
reading for research lately. I decided to talk about a super cool
trick in a field called domain theory. It's a method of generating a
solution to a large class of recursive equations.

In order to go through this idea we've got some background to cover. I
wanted to make this post readable even if you haven't read too much
domain theory (you do need to know what a functor/colimit is though,
nothing crazy though). We'll start with a whirlwind tutorial of the
math behind domain theory. From there we'll transform the problem of
finding a solution to an equation into something categorically
tractable. Finally, I'll walk through the construction of a solution.

I decided not to show an example of applying this technique to model a
language because that would warrant its own post, hopefully I'll
write about that soon :)

## Basic Domain Theory

The basic idea with domain theory comes from a simple problem. Suppose
we want to model the lambda calculus. We want a collection of
mathematical objects `D` so that we can treat element of `D` as a
function `D -> D` and each function `D -> D` as an element of
`D`. To see why this is natural, remember that we want to turn each
program `E` into `d ∈ D`. If `E = λ x. E'` then we need to turn the
function `e ↦ [e/x]E'` into a term. This means `D → D` needs to be
embeddable in `D`. On the other hand, we might have `E = E' E''` in
which case we need to turn `E'` into a function `D → D` so that we can
apply it. This means we need to be able to embed `D` into `D → D`.

After this we can turn a lambda calculus program into a specific
element of `D` and reason about its properties using the ambient
mathematical tools for `D`. This is semantics, understanding programs
by studying their meaning in some mathematical structure. In our
specific case that structure is `D` with the isomorphism `D ≅ D → D`.
However, there's an issue! We know that `D` can't just be a set
because then there cannot be such an isomorphism! In the case where `D
≅ N`, then `D → D ≅ R` and there's a nice proof by diagonalization
that such an isomorphism cannot exist.

So what can we do? We know there are only countably many programs, but
we're trying to state that there exists an isomorphism between our
programs (countable) and functions on them (uncountable). Well the
issue is that we don't really mean *all* functions on `D`, just the
ones we can model as lambda terms. For example, the function which
maps all divergent programs to `1` and all terminating ones to `0`
need not be considered because there's no lambda term for it! How do
we consider "computable" functions though? It's not obvious since we
define computable functions using the lambda calculus, what we're
trying to model here. Let's set aside this question for a moment.

Another question is how do we handle this program: `(λ x. x x) (λ x. x
x)`? It doesn't have a value after all! It doesn't behave like a
normal mathematical function because applying it to something doesn't
give us back a new term, it just runs forever! To handle this we do
something really clever. We stop considering *just* a collection of
terms and instead look at terms with an ordering relation `⊑`! The
idea is that ⊑ represents definedness. A program which runs to a value
is more defined than a program which just loops forever. Similarly,
two functions behave the same on all inputs except for `0` where one
loops we could say one is more defined than the other. What we'll do
is define ⊑ abstractly and then model programs into sets with such a
relation defined upon them. In order to build up this theory we need a
few definitions

A partially ordered set (poset) is a set `A` and a binary relation `⊑`
where

 1. `a ⊑ a`
 2. `a ⊑ b` and `b ⊑ c` implies `a ⊑ c`
 3. `a ⊑ b` and `b ⊑ a` implies `a = b`

We often just denote the pair `<A, ⊑>` as `A` when the ordering is
clear. With a poset `A`, of particular interest are chains in it. A
chain is collection of elements `aᵢ` so that `aᵢ ⊑ aⱼ` if `i ≤ j`. For
example, in the partial order of natural numbers and `≤`, a chain is
just a run of ascending numbers. Another fundamental concept is called
a least upper bound (lub). A lub of a subset `P ⊆ A` is an element of
` x ∈ A` so that `y ∈ P` implies `y ⊑ x` and if this property holds
for some `z` also in `A`, then `x ⊑ z`. So a least upper bound is just
the smallest thing bigger than the subset. This isn't always
guaranteed to exist, for example, in our poset of natural numbers `N`,
the subset `N` has no upper bounds at all! When such a lub does exist,
we denote it with `⊔P`. Some partial orders have an interesting
property, all chains in them have least upper bounds. We call this
posets *complete* partial orders or cpos.

For example while `N` isn't a cpo, `ω` (the natural numbers + an
element greater than all of them) *is*! As a quick puzzle, can you
show that all finite partial orders are in fact CPOs?

We can define a number of basic constructions on cpos. The most common
is the "lifting" operation which takes a cpo `D` and returns `D⊥`, a
cpo with a least element `⊥`. A cpo with such a least element is
called "pointed" and I'll write that as cppo (complete pointed partial
order). Another common example, given two cppos, `D` and `E`, we can
construct `D ⊗ E`. An element of this cppo is either `⊥` or `<l, r>`
where `l ∈ D - {⊥}` and `r ∈ E - {⊥}`. This is called the smash
product because it "smashes" the ⊥s out of the components. Similarly,
there's smash sums `D ⊕ E`.

The next question is the classic algebraic question to ask about a
structure: what are the interesting functions on it? We'll in
particular be interested in functions which preserve the ⊑ relation
and the taking of lub's on chains. For this we have two more
definitions:

 1. A function is monotone if `x ⊑ y` implies `f(x) ⊑ f(y)`
 2. A function is continuous if it is monotone and for all chains
    `C`, `⊔ f(P) = f(⊔ P)`.

Notably, the collection of cppos and continuous functions form a
category! This is because clearly `x ↦ x` is continuous and the
composition of two continuous functions is continuous. This category
is called `Cpo`. It's here that we're going to do most of our
interesting constructions.

Finally, we have to discuss one important construction on
`Cpo`: `D → E`. This is the set of continuous
functions from `D` to `E`. The ordering on this is pointwise, meaning
that `f ⊑ g` if for all `x ∈ D`, `f(x) ⊑ g(x)`. This is a cppo where
`⊥` is `x ↦ ⊥` and all the lubs are determined pointwise.

This gives us most of the mathematics we need to do the constructions
we're going to want, to demonstrate something cool here's a fun
theorem which turns out to be incredibly useful: Any continuous
function `f : D → D` on a cppo `D` has a least fixed point.

To construct this least point we need to find an `x` so that `x =
f(x)`. To do this, note first that `x ⊑ f(x)` by definition and by the
monotonicity of `f`: `f(x) ⊑ f(y)` if `x ⊆ y`. This means that the
collection of elements `fⁱ(⊥)` forms a chain with the ith element
being the ith iteration of `f`! Since `D` is a cppo, this chain has an
upper bound: `⊔ fⁱ(⊥)`. Moreover, `f(⊔ fⁱ(⊥)) = ⊔ f(fⁱ(⊥))` by the
continuity of `f`, but `⊔ fⁱ(⊥) = ⊥ ⊔ (⊔ f(fⁱ(⊥))) = ⊔ f(fⁱ(⊥))` so
this is a fixed point! The proof that it's a least fixed point is
elided because typesetting in markdown is a bit of a bother.

So there you have it, very, very basic domain theory. I can now answer
the question we weren't sure about before, the slogan is "computable
functions are continuous functions".

## Solving Recursive Equations in `Cpo`

So now we can get to the result showing domain theory incredibly
useful. Remember our problem before? We wanted to find a collection
`D` so that

    D ≅ D → D

However it wasn't clear how to do this due to size issues. In `Cpo`
however, we can absolutely solve this. This huge result was due to
Dana Scott. First, we make a small transformation to the problem
that's very common in these scenarios. Instead of trying to solve this
equation (something we don't have very many tools for) we're going to
instead look for the fixpoint of this functor

    F(X) = X → X

The idea here is that we're going to prove that all well behaved
endofunctors on Cpo have fixpoints. By using this viewpoint we get all
the powerful tools we normally have for reasoning about functors in
category theory. However, there's a problem: the above isn't a
functor! It has both positive and negative occurrences of `X` so it's
neither a co nor contravariant functor. To handle this we apply
another clever trick. Let's not look at endofunctors, but rather
functors `Cpoᵒ × Cpo → Cpo` (I believe this should be attributed to
Freyd). This is a binary functor which is covariant in the second
argument and contravariant in the first. We'll use the first argument
everywhere there's a negative occurrence of `X` and the second for
every positive occurrence. Take note: we need things to be
contravariant in the first argument because we're using that first
argument negatively: if we didn't do that we wouldn't have a functor.

Now we have

    F(X⁻, X⁺) = X⁻ → X⁺

This is functorial. We can also always recover the original map simply
by diagonalizing: `F(X) = F(X, X)`. We'll now look for an object `D`
so that `F(D, D) ≅ D`. Not quite a fixed point, but still equivalent
to the equation we were looking at earlier.

Furthermore, we need one last critical property, we want `F` to be
locally continuous. This means that the maps on morphisms determined
by `F` should be continuous so `F(⊔ P, g) = ⊔ F(P, g)` and vice-versa
(here `P` is a set of functions). Note that such morphisms have an
ordering because they belong to the pointwise ordered cppo we talked
about earlier.

We have one final thing to set up before this proof: what about if
there's multiple non-isomorphic solutions to `F`? We want a further
coherence condition that's going to provide us with 2 things

 1. An ability to uniquely determine a solution
 2. A powerful proof technique that isolates us from the particulars
    of the construction

What we want is called minimal invariance. Suppose we have a `D` and
an `i : D ≅ F(D, D)`. This is the minimal invariant solution if and
only if the least fixed point of `f(e) = i⁻ ∘ F(e, e) ∘ i` is `id`. In
other words, we want it to be the case that

    d = ⊔ₓ fˣ(⊥)(d) (d ∈ D)

I mentally picture this as saying that the isomorphism is set up so
that for any particular `d` we choose, if we apply `i`, `fmap` over
it, apply `i` again, repeat and repeat, eventually this process will
halt and we'll run out of things to `fmap` over. It's a sort of a
statement that each `d ∈ D` is "finite" in a very, *very* handwavy
sense. Don't worry if that didn't make much sense, it's helpful to me
but it's just my intuition. This property has some interesting effects
though: it means that if we find such a `D` then `(D, D)` is going to
be both the initial algebra and final coalgebra of `F`.

Without further ado, let's prove that every locally continuous functor
`F`. We start by defining the following

    D₀ = {⊥}
    Dᵢ  = F(Dᵢ₋₁, Dᵢ₋₁)

This gives us a chain of cppos that gradually get larger. How do we
show that they're getting larger? By defining an section from `Dᵢ` to
`Dⱼ` where `j = i + 1`. A section is a function `f` which is paired
with a (unique) function `f⁰` so that `f⁰f = id` and `ff⁰ ⊑ id`. In
other words, `f` embeds its domain into the codomain and `f⁰` tells us
how to get it out. Putting something in and taking it out is a round
trip. Since the codomain may be bigger though taking something out and
putting it back only *approximates* a round trip. Our sections are
defined thusly

    s₀ = x ↦ ⊥         r₀ = x ↦ ⊥
    sᵢ  = F(rᵢ₋₁, sᵢ₋₁)   rᵢ = F(rᵢ₋₁, sᵢ₋₁)

It would be very instructive to work out that these definitions are
actually sections and retractions. Since type-setting this subscripts
is a little rough, if it's clear from context I'll just write `r` and
`s`. Now we've got this increasing chain, we define an interesting
object

     D = {x ∈ Πᵢ Dᵢ | x.(i-1) = r(x.i)}

In other words, `D` is the collection of infinitely large pairs. Each
component if from one of those `Dᵢ`s above and they cohere with each
other so using `s` and `r` to step up the chain takes you from one
component to the next. Next we define a way to go from a single `Dᵢ`
to a `D`: `upᵢ : Dᵢ → D` where

    upᵢ(x).j =  x    if i = j
             | rᵈ(x) if i - j = d > 0
             | sᵈ(x) if j - i = d > 0

Interestingly, note that `πᵢ ∘ upᵢ = id` (easy proof) and that `upᵢ ∘
πᵢ ⊑ id` (slightly harder proof). This means that we've got more
sections lying around: every `Dᵢ` can be fed into `D`. Consider the
following diagram

        s      s      s
    D0 ——> D1 ——> D2 ——> ...

I claim that `D` is the colimit to this diagram where the collection
of arrows mapping into it are given with `upᵢ`. Seeing this is a
colimit follows from the fact that `πᵢ ∘ upᵢ` is just
`id`. Specifically, suppose we have some object `C` and a family of
morphisms `cᵢ : Dᵢ → C` which commute properly with `s`. We need to
find a unique morphism `h` so that `cᵢ = h ∘ upᵢ`. Define `h` as
`⊔ᵢ cᵢπᵢ`. Then

    h ∘ upⱼ = (⊔j<i cᵢsʲrʲ) ⊔ cᵢ ⊔ (⊔j>i cᵢrʲsʲ) = (⊔j<i cᵢsʲrʲ) ⊔ cᵢ

The last step follows from the fact that `rʲsʲ = id`. Furthermore,
`sʲrʲ ⊑ id` so `cᵢsʲrʲ ⊑ cᵢ` so that whole massive term just evaluates
to `cᵢ` as required. So we have a colimit. Notice that if we apply `F`
to each `Dᵢ` in the diagram we end up with a new diagram.


        s      s      s
    D1 ——> D2 ——> D3 ——> ...


`D` is still the colimit (all we've done is shift the diagram over by
one) but by identical reasoning to `D` being a colimit, so is `F(D,
D)`. This means we have a unique isomorphism `i : D ≅ F(D, D)`. The
fact that `i` is the minimal invariant follows from the properties we
get from the fact that `i` comes from a colimit.

With this construction we can construct our model of the lambda
calculus simply by finding the minimal invariant of the locally
continuous functor `F(D⁻, D⁺) = D⁻ → D⁺` (it's worth proving it's
locally continuous). Our denotation is defined as `[e]ρ ∈ D` where `e`
is a lambda term and `ρ` is a map of the free variables of `e` to
other elements of `D`. This is inductively defined as

    [λx. e]ρ = i⁻(d ↦ [e]ρ[x ↦ d])
    [e e']ρ = i([e]ρ)([e']ρ)
    [x]ρ = ρ(x)

Notice here that for the two main constructions we just use `i` and
`i⁻` to fold and unfold the denotations to treat them as functions. We
could go on to prove that this denotation is sound and complete but
that's something for another post.

## Wrap Up

That's the main result I wanted to demonstrate. With this single proof
we can actually model a very large class of programming languages into
`Cpo`. Hopefully I'll get around to showing how we can pull a similar
trick with a relational structure on `Cpo` in order to prove full
abstraction. This is nicely explained in Andrew Pitt's "Relational
Properties of Domains".

If you're interested in domain theory I learned from Gunter's
"Semantics of Programming Languages" book and recommend it.
