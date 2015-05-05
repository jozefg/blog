---
title: A Proof of Church Rosser in Twelf
tags: twelf, types
---

An important property in any term rewriting system, a system of rules for saying
one term can be rewritten into another, is called confluence. In a term
rewriting system more than one rule may apply at a time, confluence states that
it doesn't matter in what order we apply these rules. In other words, there's
some sort of diamond property in our system

                     Starting Term
                        /     \
                       /       \
              Rule 1  /         \ Rule 2
                     /           \
                    /             \
                   B               C
                    \              /
             A bunch \     of     / rules later
                      \          /
                       \        /
                        \      /
                     Same end point

In words (and not a crappy ascii picture)

 1. Suppose we have some term `A`
 2. The system lets us rewrite `A` to `B`
 3. The system lets us rewrite `A` to `C`

Then two things hold

 1. The system lets us rewrite `B` to `D` in some number of rewrites
 2. The system lets us rewrite `C` to `D` with a different series of rewrites

In the specific case of lambda calculus, confluence is referred to as the
"Church-Rosser Theorem". This theorem has several important corollaries,
including that the normal forms of any lambda term is unique. To see this,
remember that a normal form is always "at the bottom" of diamonds like the one
we drew above. This means that if some term had multiple steps to take, they all
must converge before one of them reaches a normal form. If any of them did hit a
normal form first, they couldn't complete the diamond.

## Proving Church-Rosser

In this post I'd like to go over a proof of the Church Rosser theorem in Twelf,
everyone's favorite mechanized metalogic. To follow along if you don't know
Twelf, perhaps some shameless [self][intro] [linking][worlds] will help.

We need to start by actually defining lambda calculus. In keeping with Twelf
style, we laugh at those restricted by the bounds of inductive types and use
higher order abstract syntax to get binding for free.

``` twelf
    term : type.
    ap   :  term -> term  -> term.
    lam  : (term -> term) -> term.
```

We have to constructors, `ap`, which applies one term to another. The
interesting one here is `lam` which embeds the LF function space, `term -> term`
into `term`. This actually makes sense because `term` isn't an inductive type,
just a type family with a few members. There's no underlying induction principle
with which we can derive contradictions. To be perfectly honest I'm not sure how
the proof of soundness of something like Twelf %total mechanism proceeds. If a
reader is feeling curious, I believe [this][total] is the appropriate paper to
read.

With this, something like `λx. x x` as `lam [x] ap x x`.

Now on to evaluation. We want to talk about things as a term rewriting system,
so we opt for a small step evaluation approach.

``` twelf
    step     : term -> term -> type.
    step/b   : step (ap (lam F) A) (F A).
    step/ap1 : step (ap F A) (ap F' A)
                <- step F F'.
    step/ap2 : step (ap F A) (ap F A')
                <- step A A'.
    step/lam : step (lam [x] M x) (lam [x] M' x)
                <- ({x} step (M x) (M' x)).

    step* : term -> term -> type.
    step*/z : step* A A.
    step*/s : step* A C
               <- step A B
               <- step* B C.
```

We start with the 4 sorts of steps you can make in this system. 3 of them are
merely "if you can step somewhere else, you can pull the rewrite out", I've
heard these referred to as compatibility rules. This is what `ap1`, `ap2` and
`lam` do, `lam` being the most interesting since it deals with going under a
binder. Finally, the main rule is `step/b` which defines beta reduction. Note
that HOAS gives us this for free as application.

Finally, `step*` is for a series of steps. We either have no steps, or a step
followed by another series of steps. Now we want to prove a couple theorems
about our system. These are mostly the lifting of the "compatibility rules" up
to working on `step*`s. The first is the lifting of `ap1`.

``` twelf
     step*/left : step* F F' -> step* (ap F A) (ap F' A) -> type.
     %mode +{F : term} +{F' : term} +{A : term} +{In : step* F F'}
     -{Out : step* (ap F A) (ap F' A)} (step*/left In Out).

     - : step*/left step*/z step*/z.
     - : step*/left (step*/s S* S) (step*/s S'* (step/ap1 S))
          <- step*/left S* S'*.

     %worlds (lam-block) (step*/left _ _).
     %total (T) (step*/left T _).
```

*Note, the mode specification I'm using a little peculiar. It needs
to be this verbose because otherwise `A` mode-errors. Type inference
is peculiar.*

The theorem says that if `F` steps to `F'` in several steps, for all `A`,
`ap F A` steps to `ap F' A` in many steps. The actual proof is quite boring,
we just recurse and apply `step/ap1` until everything type checks.

Note that the world specification for `step*/left` is a little strange. We use
the block `lam-block` because later one of our theorem needs this. The block is
just

    %block lam-block : block {x : term}.

We need to annotate this on all our theorems because Twelf's world subsumption
checker isn't convinced that `lam-block` can subsume the empty worlds we check
some of our theorems in. Ah well.

Similarly to `step*/left` there is `step*/right`. The proof is 1 character off
so I won't duplicate it.

``` twelf
    step*/right : step* A A' -> step* (ap F A) (ap F A') -> type.
````

Finally, we have `step/lam`, the lifting of the compatibility rule for
lambdas. This one is a little more fun since it actually works by pattern
matching on functions.

``` twelf
     step*/lam : ({x} step* (F x) (F' x))
                  -> step* (lam F) (lam F')
                  -> type.
     %mode step*/lam +A -B.

     - : step*/lam ([x] step*/z) step*/z.
     - : step*/lam ([x] step*/s (S* x) (S x))
          (step*/s S'* (step/lam S))
          <- step*/lam S* S'*.

     %worlds (lam-block) (step*/lam _ _).
     %total (T) (step*/lam T _).
```

What's fun here is that we're inducting on a dependent function. So the first
case matches `[x] step*/z` and the second `[x] step*/s (S* x) (S x)`. Other than
that we just use `step/lam` to lift up `S` and recurse to lift up `S*` in the
second case.

We need one final (more complicated) lemma about substitution. It states that
if `A` steps to `A'`, then `F A` steps to `F A'` in many steps for all `F`. This
proceeds by induction on the derivation that `A` steps to `A'`. First off,
here's the formal statement in Twelf

*This is the lemma that actually needs the world with `lam-block`s*

``` twelf
    subst : {F} step A A' -> step* (F A) (F A') -> type.
    %mode subst +A +B -C.
```

Now the actual proof. The first two cases are for constant functions and the
identity function

``` twelf
    - : subst ([x] A) S step*/z.
    - : subst ([x] x) S (step*/s step*/z S).
```

In the case of the constant functions the results of `F A` and `F A'` are the
same so we don't need to step at all. In the case of the identity function we
just step with the step from `A` to `A'`.

In the next case, we deal with nested lambdas.

``` twelf
     - : subst ([x] lam ([y] F y x)) S S'*
          <- ({y} subst (F y) S (S* y))
          <- step*/lam S* S'*.
```

Here we recurse, but we carefully do this under a pi type. The reason for doing
this is because we're recursing on the open body of the inner lambda. This has a
free variable and we need a pi type in order to actually apply `F` to something
to get at the body. Otherwise this just uses `step*/lam` to lift the step across
the body to the step across lambdas.

Finally, application.

``` twelf
     - : subst ([x] ap (F x) (A x)) S S*
          <- subst F S F*
          <- subst A S A*
          <- step*/left F* S1*
          <- step*/right A* S2*
          <- join S1* S2* S*.
```

This looks complicated, but isn't so bad. We first recurse, and then use various
compatibility lemmas to actually plumb the results of the recursive calls to the
right parts of the final term. Since there are two individual pieces of
stepping, one for the argument and one for the function, we use `join` to slap
them together.

With this, we've got all our lemmas

``` twelf
    %worlds (lam-block) (subst _ _ _).
    %total (T) (subst T _ _).
```

## The Main Theorem

Now that we have all the pieces in place, we're ready to state and prove
confluence. Here's our statement in Twelf

``` twelf
    confluent : step A B -> step A C -> step* B D -> step* C D -> type.
    %mode confluent +A +B -C -D.
```

Unfortunately, there's a bit of a combinatorial explosion with this. There are
approximately 3 * 3 * 3 + 1 = 10 cases for this theorem. And thanks to the
lemmas we've proven, they're all boring.

First we have the cases where `step A B` is a `step/ap1`.

``` twelf
     - : confluent (step/ap1 S1) (step/ap1 S2) S1'* S2'*
          <- confluent S1 S2 S1* S2*
          <- step*/left S1* S1'*
          <- step*/left S2* S2'*.
     - : confluent (step/ap1 S1) (step/ap2 S2)
          (step*/s step*/z (step/ap2 S2))
          (step*/s step*/z (step/ap1 S1)).
     - : confluent (step/ap1 (step/lam F) : step (ap _ A) _) step/b
          (step*/s step*/z step/b) (step*/s step*/z (F A)).
```

In the first case, we have two `ap1`s. We recurse on the smaller `S1` and `S2`
and then immediately use one of our lemmas to lift the results of the recursive
call, which step the function part of the the `ap` we're looking at, to work
across the whole `ap` term. In the second case there, we're stepping the
function in one, and the argument in the other. In order to bring these to a
common term we just apply the first step to the resulting term of the second
step and vice versa. This means that we're doing something like this


                     F A
                    /   \
               S1  /     \ S2
                  /       \
                F' A     F  A'
                  \       /
               S2  \     /  S1
                    \   /
                    F' A'

This clearly commutes so this case goes through. For the final case, we're
applying a lambda to some term so we can beta reduce. On one side we step the
body of the lambda some how, and on the other we immediately substitute. Now we
do something clever. What is a proof that `lam A` steps to `lam B`? It's a proof
that for any `x`, `A x` steps to `B x`. In fact, it's just a function from `x`
to such a step `A x` to `B x`. So we have that lying around in `F`. So to step
from the beta-reduced term `G A` to `G' A` all we do is apply `F` to `A`! The
other direction is just beta-reducing `ap (lam G') A` to the desired `G' A`.

In the next set of cases we deal with `ap2`!

``` twelf
     - : confluent (step/ap2 S1) (step/ap2 S2) S1'* S2'*
          <- confluent S1 S2 S1* S2*
          <- step*/right S1* S1'*
          <- step*/right S2* S2'*.
     - : confluent (step/ap2 S1) (step/ap1 S2)
          (step*/s step*/z (step/ap1 S2))
          (step*/s step*/z (step/ap2 S1)).
     - : confluent (step/ap2 S) (step/b : step (ap (lam F) _) _)
          (step*/s step*/z step/b) S1*
          <- subst F S S1*.
```

The first two cases are almost identical to what we've seen before. The key
difference here is in the third case. This is again where we're stepping
something on one side and beta-reducing on the other. We can't use the nice free
stepping provided by `F` here since we're stepping the argument, not the
function. For this we appeal to `subst` which let's us step `F A` to `F A'`
using `S1*` exactly as required. The other direction is trivial just like it was
in the `ap1` case, we just have to step `ap (lam F) A'` to `F A'` which is done
with beta reduction.

I'm not going to detail the cases to do with `step/b` as the first argument
because they're just mirrors of the cases we've looked at before. That only
leaves us with one more case, the case for `step/lam`.

``` twelf
     - : confluent (step/lam F1) (step/lam F2) F1'* F2'*
          <- ({x} confluent (F1 x) (F2 x) (F1* x) (F2* x))
          <- step*/lam F1* F1'*
          <- step*/lam F2* F2'*.
```

This is just like all the other "diagonal" cases, like
`confluent (ap1 S1) (ap1 S2) ...`. We first recurse (this time using a pi to
unbind the body of the lambda) and then use compatibility rules in order to get
something we can give back from `confluent`. And with this, we can actually
prove that lambda calculus is confluent.

``` twelf
    %worlds (lam-block) (confluent _ _ _ _).
    %total (T) (confluent T _ _ _).
```

## Wrap Up

We went through a fairly significant proof here, but the end results were
interesting at least. One nice thing this proof illustrates is how well HOAS
lets us encode these proofs. It's a very Twelf-y approach to use lambdas to
represent bindings. All in all, it's a fun proof.


[intro]: /posts/2015-02-28-twelf.html
[worlds]: /posts/2015-03-07-worlds.html
[total]: http://repository.cmu.edu/cgi/viewcontent.cgi?article=2238&context=compsci
