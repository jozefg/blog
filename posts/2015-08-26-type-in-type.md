---
title: Type is not in Type
tags: jonprl, types, haskell
---

I was reading a [recent proposal][dep-haskell] to merge types and
kinds in Haskell to start the transition to dependently typed
Haskell. One thing that caught my eye as I was reading it was that
this proposal adds `* :: *` to the type system. This is of some
significance because it means that once this is fully realized Haskell
will be inconsistent (as a logic) in a new way! Of course, this isn't
a huge deal since Haskell is already woefully inconsistent with

 - `unsafePerformIO`
 - Recursive bindings
 - Recursive types
 - Exceptions
 - ...

So it's not like we'll be entering new territory here. All that it
means is that there's a new way to inhabit every type in
Haskell. If you were using Haskell as a proof assistant you were
already in for a rude awakening I'm afraid :)

This is an issue of significance though for languages like Idris or
Agda where such a thing would actually render proofs
useless. Famously, Martin-Lof's original type theory did have `Type :
Type` (or `* :: *` in Haskell spelling) and Girard managed to derive a
contradiction (Girard's paradox). I've always been told that the
particulars of this construction are a little bit complicated but to
remember that `Type : Type` is bad.

In this post I'd like to prove that `Type : Type` is a contradiction
in [JonPRL][jonprl]. This is a little interesting because in most
proof assistants this would work in two steps

 1. Hack the compiler to add the rule `Type : Type`
 2. Construct a contradiction and check it with the modified compiler

*OK to be fair, in something like Agda you could use the compiler
 hacking they've already done and just say `{-# OPTIONS --set-in-set
 #-}` or whatever the flag is. The spirit of the development is the
 same though*

In JonPRL I'm just going to prove this as a regular implication. We
have a proposition which internalizes membership and I'll demonstrate
`not(member(U{i}; U{i}))` is provable (`U{i}` is how we say `Type` in
JonPRL). It's the same logic as we had before.

## Background on JonPRL

Before we can really get to the proof we want to talk about we should
go through some of the more advanced features of JonPRL we need to
use.

JonPRL is a little different than most proof assistants, for example
We can define a type of all closed terms in our language and whose
equality is purely computational. This type is `base`. To prove that
`=(a; b; base)` holds you have to prove `ceq(a; b)`, the finest grain
equality in JonPRL. Two terms are `ceq` if they

 1. Both diverge
 2. Run to the same outermost form and have `ceq` components

What's particularly exciting is that you can substitute any term for
any other term `ceq` to it, no matter at what type it's being used. In
fact, the `reduce` tactic (which beta reduces terms) can conceptually
be thought of as substituting a bunch of terms for their whn forms
which are `ceq` to the original terms. The relevant literature behind
this is found in Doug Howe's "Equality in a Lazy Computation
System". There's more in JonPRL in this regard, we also have the
asymmetric version of `ceq` (called `approx`) but we won't need it
today.

Next, let's talk about the image type. This is a type constructor
with the following formation rule

     H ⊢ A : U{i}        H ⊢ f : base
     —————————————————————————————————
          H ⊢ image(H; f) : U{i}

So here `A` is a type and `f` is anything. Things are going to be
equal `image` if we can prove that they're of the form `f w` and `f
w'` where `w = w' ∈ A`. So `image` gives us the codomain (range) of a
function. What's pretty crazy about this is that it's not just the
range of some function `A → B`, we don't really need a whole new type
for that. It's the range of *literally any closed term we can
apply*. We can take the range of the y combinator over pi types. We
can take the range of `lam(x. ⊥)` over `unit`, anything we want!

This construct let's us define some really incredible things as a user
of JonPRL. For example, the "squash" of a type is supposed to be a
type which is occupied by `<>` (and only `<>`) if and only if there
was an occupant of the original type. You can define these in HoTT
with higher inductive types. Or, you can define these in this type
theory as

``` jonprl
    Operator squash : (0).
    [squash(A)] =def= [image(A; lam(x. <>))]
```

`x ∈ squash(A)` if and only if we can construct an `a` so
that `a ∈ A` and `lam(x. <>) a ~ x`. Clearly `x` must be `<>` and we
can construct such an `a` if and only if `A` is nonempty.

We can also define the set-union of two types. Something is supposed
to be in the set union if and only if it's in one or the other. Two
define such a thing with an image type we have

``` jonprl
    Operator union : (0).
    [union(A; B)] =def= [image((x : unit + unit) * decide(x; _.A; _.B); lam(x.snd(x)))]
```

This one is a bit more complicated. The domain of things we're
applying our function to this time is

``` jonprl
    (x : unit + unit) * decide(x; _.A; _.B)
```

This is a dependent pair, sometimes called a Σ type. The first
component is a boolean, if the first component is true the second
component is of type `A`, otherwise it's of type `B`. So for every
term of type `A` or `B`, there's a term of this Σ type. In fact, we
can recover that original term of type `A` or `B` by just grabbing the
second component of the term! We don't have to worry about the type of
such an operation because we're not creating something with a function
type, just something in `base`.

`union`s let us define an absolutely critical admissible rule in our
system. See JonPRL has this propositional reflection of the equality
judgment and membership, but membership is non-negatable. By this I
mean that if we have some `a` so that `a = a ∈ A` doesn't hold, we
won't be able to prove `=(a; a; A) -> void`. See in order to prove
such a thing we first have to prove that `=(a; b; A) -> void` is a
type, which means proving that `=(a; a; A)` is a type.

In order to prove that `=(a; b; A)` is a proposition we have to prove
`=(a; a; A)`, `=(b; b; A)`, and `=(A; A; U{i})`. The process of
proving these will actually also show that the corresponding
judgments, `a ∈ A`, `b ∈ A`, and `A ∈ U{i}` hold.

However, in the case that `a` and `b` are the same term this is just
the same as proving `=(a; b; A)`! So `=(a; a; A)` is a proposition
only if it's true. However, we can add a rule that says that `=(a; b;
A)` is a proposition if `a = a ∈ (A ∪ base)` and similarly for `b`!
This fixes our negatibility issue because we can just prove that `=(a;
a; base)`, something that may be true even if `a` is not equal in
`A`. Before having a function take a `member(...)` was useless
(`member(a; A)` is just thin sugar for `=(a; a; A)`!  `member(a; A)`
is a proposition if and only if `a = a ∈ A` holds, in other words,
it's a proposition if and only if it's true! With this new rule, we
can prove `member(a; A)` is a proposition if `A ∈ U{i}` and `a ∈
base`, a much weaker set of conditions that are almost always true. We
can apply this special rule in JonPRL with `eq-eq-base` instead of
just `eq-cd` like the rest of our equality rules.

## The Main Result

Now let's actually begin proving Russell's paradox. To start with some
notation.

``` jonprl
    Infix 20 "∈" := member.
    Infix 40 "~" := ceq.
    Infix 60 "∪" := bunion.
    Prefix 40 "¬" := not.
```

This let's us say `a ∈ b` instead of `member(a; b)`. JonPRL recently
grew this ability to add transparent notation to terms, it makes our
theorems a *lot* prettier.

Next we define the central term to our proof

``` jonprl
    Operator Russell : ().
    [Russell] =def= [{x : U{i} | ¬ (x ∈ x)}]
```

Here we've defined `Russell` as shorthand for a subset type, in
particular it's a subset of `U{i}` (the universe of types). `x ∈
Russell` if `x ∈ U{i}` and `¬ (x ∈ x)`. Now normally we won't be able
to prove that this is a type (specifically `x ∈ x` is going to be a
problem), but in our case we'll have some help from an assumption that
`U{i} ∈ U{i}`.

Now we begin to define a small set of tactic that we'll want. These
tactics are really where the fiddly bits of using JonPRL's tactic
system come into play. If you're just reading this for the intuition
as to why `Type ∈ Type` is bad just skip this. You'll still understand
the construction even if you don't understand these bits of the proof.

First we have a tactic which finds an occurrence of `H : A + B` in the
context and eliminate it. This gives us two goals, one with an `A` and
one with a `B`. To do this we use match, This gives us something like
`match goal with` in Coq.

``` jonprl
    Tactic break-plus {
      @{ [H : _ + _ |- _] => elim <H>; thin <H> }
    }.
```

Note the syntax `[H : ... |- ...]` to match on a sequent. In
particular here we just have `_ + _` and `_`. Next we have a tactic
`bunion-eq-right`. It's to help us work with `bunion`s
(unions). Basically it turns `=(M; N; bunion(A; B))` into

``` jonprl
    =(lam(x.snd(x)) <<>, M>; lam(x.snd(x)) <<>, N>; bunion(A; B))
```

This is actually helpful because it turns out that once we unfold
`bunion` we have to prove that `M` and `N` are in an image type,
remember that `bunion` is just a thin layer of sugar on top of image
types. In order to prove something is in the image type it needs to be
of the form `f a` where `f` in our case is `lam(x. snd(x))`.

This is done with

``` jonprl
    Tactic bunion-eq-right {
      @{ [|- =(M; N; L ∪ R)] =>
           csubst [M ~ lam(x. snd(x)) <inr(<>), M>] [h.=(h;_;_)];
           aux { unfold <snd>; reduce; auto };
           csubst [N ~ lam(x. snd(x)) <inr(<>), N>] [h.=(_;h;_)];
           aux { unfold <snd>; reduce; auto };
      }
    }.
```

The key here is `csubst`. It takes a `ceq` as its first argument and a
"targeting". It then tries to replace each occurrence of the left side
of the equality with the right. To find each occurrence the targeting
maps a variable to each occurrence. We're allowed to use wildcards in
the targeting as well. It also relegates actually proving the equality
into a new subgoal. It's easy enough to prove so we demonstrate it
with `aux {unfold <snd>; reduce; auto}`.

We only need to apply this tactic after `eq-eq-base`, this applies
that rule I mentioned earlier about proving equalities well formed in
a much more liberal environment. Therefore we wrap those two tactics
into one more convenient package.

``` jonprl
    Tactic eq-base-tac {
      @{ [|- =(=(M; N; A); =(M'; N'; A'); _)] =>
           eq-eq-base; auto;
           bunion-eq-right; unfold <bunion>
       }
    }.
```

There is one last tactic in this series, this one to prove that
`member(X; X) ∈ U{i'}` is well formed (a type). It starts by unfolding
`member` into `=(=(X; X; X); =(X; X; X); U{i})` and then applying the
new tactic. Then we do other things. These things aren't pretty. I
suggest we just ignore them.

``` jonprl
    Tactic impredicativity-wf-tac {
      unfold <member>; eq-base-tac;
      eq-cd; ?{@{[|- =(_; _; base)] => auto}};
      eq-cd @i'; ?{break-plus}; reduce; auto
    }.
```

Finally we have a tactic to prove that if we have `not(P)` and `P`
existing in the context proves `void`. This is another nice
application match

``` jonprl
    Tactic contradiction {
      unfold <not implies>;
      @{ [H : P -> void, H' : P |- void] =>
           elim <H> [H'];
           unfold <member>;
           auto
       }
    }.
```

We start by unfolding `not` and `implies`. This gives us `P -> void`
and `P`. From there, we just apply one to the other giving us a void
as we wanted.

We're now ready to prove our theorem. We start with

``` jonprl
    Theorem type-not-in-type : [¬ (U{i} ∈ U{i})] {
    }.
```

We now have the main subgoal

    Remaining subgoals:

    [main] ⊢ not(member(U{i}; U{i}))

We can start by unfold `not` and `implies`. Remember that `not` isn't
a built in thing, it's just sugar. By unfolding it we get the more
primitive form, something that actually apply the `intro` tactic to.

``` jonprl
    {
      unfold <not implies>; intro
    }
```

Once unfolded, we'd get a goal along the lines of `member(U{i}; U{i})
-> void`. We immediately apply `intro` to this though. Now we have two
subgoals, one is the result of applying `intro`, namely we have a
hypothesis `x : member(U{i}; U{i})` and a goal `void`. The second
subgoal is the "well-formedness" obligation.

We have to prove that `member(U{i}; U{i})` is a type in order to apply
the `intro` tactic. This is a crucial difference between Coq-like
systems and these proof-refinement logics. The process of
demonstrating that what you're proving is a proposition is
intermingled with actually constructing the proof. It means you get to
apply all the normal mathematical tools you have for proving things to
be true to prove that they're types. This gives us a lot of
flexibility, but at the cost of sometimes annoying subgoals.  They're
annotated with `[aux]` (as opposed to `[main]`). This means we can
target them all at once using with the `aux` tactics.

To summarize that whole paragraph as JonPRL would say it, our proof
state is

    [main]
    1. x : member(U{i}; U{i})
    ⊢ void

    [aux] ⊢ member(member(U{i}; U{i}); U{i'})

Let's get rid of that auxiliary subgoal using that
`impredictivity-wf-tac`, this subgoal is in fact exactly what it was
made for.

``` jonprl
    {
      unfold <not implies>; intro
      aux { impredicativity-wf-tac };
    }
```

This picks off that `[aux]` goal leaving us with just

    [main]
    1. x : member(U{i}; U{i})
    ⊢ void

Now we need to prove some lemmas. They state that `Russell` is
actually a type. This is possible to do here and only here because
we'll need to actually use `x` in the process of proving this. It's a
very nice example of what explicitly proving well-formedness can give
you! After all, the process of demonstrating Russell is a type is
nontrivial and only true in this hypothetical context, rather than
just hoping that JonPRL is clever enough to figure that out for itself
we get to demonstrate it locally.

We're going to use the `assert` tactic to get these lemmas. This lets
us state a term, prove it as a subgoal and use it as a hypothesis in
the main goal. If you're logically minded, it's cut.

``` jonprl
    {
      unfold <not implies>; intro;
      aux { impredicativity-wf-tac };

      assert [Russell ∈ U{i}] <russell-wf>;
    }
```

The thing in `<>`s is the name it will get in our hypothetical context
for the main goal. This leaves us with two subgoals. The `aux` one
being the assertion and the `main` one being allowed to assume it.

    [aux]
    1. x : member(U{i}; U{i})
    ⊢ member(Russell; U{i})

    [main]
    1. x : member(U{i}; U{i})
    2. russell-wf : member(Russell; U{i})
    ⊢ void

We can prove this by basically working our way towards using
`impredicativity-wf-tac`. We'll use `aux` again to target the `aux`
subgoal. We'll start by unfolding everything and applying `eq-cd`.

``` jonprl
    {
      unfold <not implies>; intro;
      aux { impredicativity-wf-tac };

      assert [Russell ∈ U{i}] <russell-wf>;
      aux {
        unfold <member Russell>; eq-cd; auto;
      };
    }
```

*Remember that `Russell` is `{x : U{i} | ¬ (x ∈ x)}`*

So we just applied `eq-cd` to a subset type (`Russell`) so we get two
subgoals. One says that `U{i}` is a type, one says that if `x ∈ U{i}`
then `¬ (x ∈ x)` is also a type. In essence this just says that a
subset type is a type if both components are types. The former goal is
quite straightforward so we applied `auto` and take care of it. Now we
have one new subgoal to handle

    [main]
    1. x : =(U{i}; U{i}; U{i})
    2. x' : U{i}
    ⊢ =(not(member(x'; x')); not(member(x'; x')); U{i})

    [main]
    1. x : member(U{i}; U{i})
    2. russell-wf : member(Russell; U{i})
    ⊢ void

The second subgoal is just the rest of the proof, the first subgoal is
what we want to handle. It says that if we have a type `x` then
`not(member(x; x))` is a type albeit in ugly notation. To prove this
we have to unfold `not`. So we'll do this and apply `eq-cd` again.

``` jonprl
    {
      unfold <not implies>; intro;
      aux { impredicativity-wf-tac };

      assert [Russell ∈ U{i}] <russell-wf>;
      aux {
        unfold <member Russell>; eq-cd; auto;
        unfold <not implies>; eq-cd; auto;
      };
    }
```

Remember that `not(P)` desugars to `P -> void`. Applying `eq-cd` is
going to give us two subgoals, `P` is a type and `void` is a
type. However, `member(void; U{i})` is pretty easy to prove, so we
apply `auto` again which takes care of one of our two new goals. Now
we just have

    [main]
    1. x : =(U{i}; U{i}; U{i})
    2. x' : U{i}
    ⊢ =(member(x'; x'); member(x'; x'); U{i})

    [main]
    1. x : member(U{i}; U{i})
    2. russell-wf : member(Russell; U{i})
    ⊢ void

Now we're getting to the root of the issue. We're trying to prove that
`member(x'; x')` is a type. This is happily handled by
`impredicativity-wf-tac` which will use our assumption that `U{i} ∈
U{i}` because it's smart like that.

``` jonprl
    {
      unfold <not implies>; intro;
      aux { impredicativity-wf-tac };

      assert [Russell ∈ U{i}] <russell-wf>;
      aux {
        unfold <member Russell>; eq-cd; auto;
        unfold <not implies>; eq-cd; auto;
        impredicativity-wf-tac
      };
    }
```

Now we just have that main goal with the assumption `russell-wf`
added.

    [main]
    1. x : member(U{i}; U{i})
    2. russell-wf : member(Russell; U{i})
    ⊢ void

Now we have a similar well-formedness goal to assert and prove. We
want to prove that `∈(Russell; Russell)` is a type. This is easier
though, we can prove it easily using `impredicativity-wf-tac`.

``` jonprl
    {
      unfold <not implies>; intro;
      aux { impredicativity-wf-tac };

      assert [Russell ∈ U{i}] <russell-wf>;
      aux {
        unfold <member Russell>; eq-cd; auto;
        unfold <not implies>; eq-cd; auto;
        impredicativity-wf-tac
      };

      assert [(Russell ∈ Russell) ∈ U{i}] <russell-in-russell-wf>;
      aux { impredicativity-wf-tac; cum @i; auto };
    }
```

That `cum @i` is a quirk of `impredicativity-wf-tac`. It basically
means that instead of proving `=(...; ...; U{i'})` we can prove
`=(...; ...; U{i})` since `U{i}` is a universe below `U{i'}` and all
universes are cumulative.

Our goal is now

    [main]
    1. x : member(U{i}; U{i})
    2. russell-wf : member(Russell; U{i})
    3. russell-in-russell-wf : member(member(Russell; Russell); U{i})
    ⊢ void


Ok, so now the reasoning can start now that we have all these
well-formedness lemmas. Our proof sketch is basically as follows

 1. Prove that `Russell ∈ Russell` is false. This is because if
    `Russell` *was* in `Russell` then by definition of `Russell` it
    isn't in `Russell`.
 2. Since `not(Russell ∈ Russell)` holds, then `Russell ∈ Russell`
    holds.
 3. Hilarity ensues.

Here's the first assertion

``` jonprl
    {
      unfold <not implies>; intro;
      aux { impredicativity-wf-tac };

      assert [Russell ∈ U{i}] <russell-wf>;
      aux {
        unfold <member Russell>; eq-cd; auto;
        unfold <not implies>; eq-cd; auto;
        impredicativity-wf-tac
      };

      assert [(Russell ∈ Russell) ∈ U{i}] <russell-in-russell-wf>;
      aux { impredicativity-wf-tac; cum @i; auto };

      assert [¬ (Russell ∈ Russell)] <not-russell-in-russell>;
    }
```

Here are our subgoals

    [aux]
    1. x : member(U{i}; U{i})
    2. russell-wf : member(Russell; U{i})
    3. russell-in-russell-wf : member(member(Russell; Russell); U{i})
    ⊢ not(member(Russell; Russell))

    [main]
    1. x : member(U{i}; U{i})
    2. russell-wf : member(Russell; U{i})
    3. russell-in-russell-wf : member(member(Russell; Russell); U{i})
    4. russell-not-in-russell : not(member(Russell; Russell))
    ⊢ void

 We want to prove that first one. To start, let's unfold that `not`
 and move `member(Russell; Russell)` to the hypothesis and use it to
 prove `void`. We do this with `intro`.

``` jonprl
    {
      unfold <not implies>; intro;
      aux { ... };

      assert [Russell ∈ U{i}] <russell-wf>;
      aux { ... };

      assert [(Russell ∈ Russell) ∈ U{i}] <russell-in-russell-wf>;
      aux { ... };

      assert [¬ (Russell ∈ Russell)] <not-russell-in-russell>;
      aux {
        unfold <not implies>;
        intro @i; aux {assumption};
      }
    }
```

Notice that the well-formedness goal that `intro` is handled by our
assumption! After all, it's just `member(Russell; Russell) ∈ U{i}`, we
already proved it. Now our subgoals look like this

    [main]
    1. x : member(U{i}; U{i})
    2. russell-wf : member(Russell; U{i})
    3. russell-in-russell-wf : member(member(Russell; Russell); U{i})
    4. x' : member(Russell; Russell)
    ⊢ void

    [main]
    1. x : member(U{i}; U{i})
    2. russell-wf : member(Russell; U{i})
    3. russell-in-russell-wf : member(member(Russell; Russell); U{i})
    4. russell-not-in-russell : not(member(Russell; Russell))
    ⊢ void

So what we want to do is note that we've assume that `Russell ∈
Russell` we can say that `Russell ~ X` for some `X` where `¬ (X ∈ X)`
holds. But wait, that means that `¬ (Russell ∈ Russell)` holds but
we've assume that `Russell ∈ Russell` also holds so we have a
contradiction.

Let's start by introducing that `X` (here called `R`). We'll assert an
`R : Russell` such that `R ~ Russell`. We do this using dependent
pairs (here written `(x : A) * B(x)`).

``` jonprl
    {
      unfold <not implies>; intro;
      aux { ... };

      assert [Russell ∈ U{i}] <russell-wf>;
      aux { ... };

      assert [(Russell ∈ Russell) ∈ U{i}] <russell-in-russell-wf>;
      aux { ... };

      assert [¬ (Russell ∈ Russell)] <not-russell-in-russell>;
      aux {
        unfold <not implies>;
        intro @i; aux {assumption};
        assert [(R : Russell) * R ~ Russell] <R-with-prop>;
        aux {
          intro [Russell] @i; auto
        };
      }
    }
```

We've proven this by `intro`. For proving dependent products we
provide an explicit witness for the first component. Basically to
prove `(x : A) * B(x)` we say `intro[Foo]`. We then have a goal
`Foo ∈ A` and `B(Foo)`. Since subgoals are fully independent of each
other, we have to give the witness for the first component
upfront. It's a little awkward, Jon's working on it :)

In this case we use `intro [Russell]`. After this we have to prove
that this witness has type `Russell` and then prove the second
component holds. Happily, `auto` takes care of both of these
obligations so `intro [Russell] @i; auto` handles it all.

Now we promptly eliminate this pair. It gives us two new facts, that
`R : Russell` and `R ~ Russell` hold.

``` jonprl
    {
      unfold <not implies>; intro;
      aux { ... };

      assert [Russell ∈ U{i}] <russell-wf>;
      aux { ... };

      assert [(Russell ∈ Russell) ∈ U{i}] <russell-in-russell-wf>;
      aux { ... };

      assert [¬ (Russell ∈ Russell)] <not-russell-in-russell>;
      aux {
        unfold <not implies>;
        intro @i; aux {assumption};
        assert [(R : Russell) * R ~ Russell] <R-with-prop>;
        aux {
          intro [Russell] @i; auto
        };

        elim <R-with-prop>; thin <R-with-prop>
      }
    }
```

This leaves our goal as

    [main]
    1. x : member(U{i}; U{i})
    2. russell-wf : member(Russell; U{i})
    3. russell-in-russell-wf : member(member(Russell; Russell); U{i})
    4. x' : member(Russell; Russell)
    5. s : Russell
    6. t : ceq(s; Russell)
    ⊢ void

    [main]
    1. x : member(U{i}; U{i})
    2. russell-wf : member(Russell; U{i})
    3. russell-in-russell-wf : member(member(Russell; Russell); U{i})
    4. russell-not-in-russell : not(member(Russell; Russell))
    ⊢ void

Now let's invert on that `s : Russell`, we want to use it to conclude
that `¬ (s ∈ s)` holds since that will give us `¬ (R ∈ R)`.

``` jonprl
    {
      unfold <not implies>; intro;
      aux { ... };

      assert [Russell ∈ U{i}] <russell-wf>;
      aux { ... };

      assert [(Russell ∈ Russell) ∈ U{i}] <russell-in-russell-wf>;
      aux { ... };

      assert [¬ (Russell ∈ Russell)] <not-russell-in-russell>;
      aux {
        unfold <not implies>;
        intro @i; aux {assumption};
        assert [(R : Russell) * R ~ Russell] <R-with-prop>;
        aux {
          intro [Russell] @i; auto
        };

        elim <R-with-prop>; thin <R-with-prop>;
        unfold <Russell>; elim #5;
      }
    }
```

Now that we've unfolded all of those `Russell`s our goal is a little
bit harder to read, remember to mentally substitute `{x : U{i} |
not(member(x; x))}` as `Russell`.

    [main]
    1. x : member(U{i}; U{i})
    2. russell-wf : member({x:U{i} | not(member(x; x))}; U{i})
    3. russell-in-russell-wf : member(member({x:U{i} | not(member(x; x))}; {x:U{i} | not(member(x; x))}); U{i})
    4. x' : member({x:U{i} | not(member(x; x))}; {x:U{i} | not(member(x; x))})
    5. s : {x:U{i} | not(member(x; x))}
    6. x'' : U{i}
    7. [t'] : not(member(x''; x''))
    8. t : ceq(x''; {x:U{i} | not(member(x; x))})
    ⊢ void

    [main]
    1. x : member(U{i}; U{i})
    2. russell-wf : member(Russell; U{i})
    3. russell-in-russell-wf : member(member(Russell; Russell); U{i})
    4. russell-not-in-russell : not(member(Russell; Russell))
    ⊢ void

Now we use #7 to derive that `not(member(Russell; Russell))` holds.

``` jonprl
    {
      unfold <not implies>; intro;
      aux { ... };

      assert [Russell ∈ U{i}] <russell-wf>;
      aux { ... };

      assert [(Russell ∈ Russell) ∈ U{i}] <russell-in-russell-wf>;
      aux { ... };

      assert [¬ (Russell ∈ Russell)] <not-russell-in-russell>;
      aux {
        unfold <not implies>;
        intro @i; aux {assumption};
        assert [(R : Russell) * R ~ Russell] <R-with-prop>;
        aux {
          intro [Russell] @i; auto
        };

        elim <R-with-prop>; thin <R-with-prop>;
        unfold <Russell>; elim #5;

        assert [¬ member(Russell; Russell)];
        aux {
          unfold <Russell>;
        };
      }
    }
```

This leaves us with 3 subgoals. The first one being the assertion.

    [aux]
    1. x : member(U{i}; U{i})
    2. russell-wf : member({x:U{i} | not(member(x; x))}; U{i})
    3. russell-in-russell-wf : member(member({x:U{i} | not(member(x; x))}; {x:U{i} | not(member(x; x))}); U{i})
    4. x' : member({x:U{i} | not(member(x; x))}; {x:U{i} | not(member(x; x))})
    5. s : {x:U{i} | not(member(x; x))}
    6. x'' : U{i}
    7. [t'] : not(member(x''; x''))
    8. t : ceq(x''; {x:U{i} | not(member(x; x))})
    ⊢ not(member({x:U{i} | not(member(x; x))}; {x:U{i} | not(member(x; x))}))

    [main]
    1. x : member(U{i}; U{i})
    2. russell-wf : member({x:U{i} | not(member(x; x))}; U{i})
    3. russell-in-russell-wf : member(member({x:U{i} | not(member(x; x))}; {x:U{i} | not(member(x; x))}); U{i})
    4. x' : member({x:U{i} | not(member(x; x))}; {x:U{i} | not(member(x; x))})
    5. s : {x:U{i} | not(member(x; x))}
    6. x'' : U{i}
    7. [t'] : not(member(x''; x''))
    8. t : ceq(x''; {x:U{i} | not(member(x; x))})
    9. H : not(member(Russell; Russell))
    ⊢ void

    [main]
    1. x : member(U{i}; U{i})
    2. russell-wf : member(Russell; U{i})
    3. russell-in-russell-wf : member(member(Russell; Russell); U{i})
    4. russell-not-in-russell : not(member(Russell; Russell))
    ⊢ void

Now to prove this what we need to do is substitute the unfolded
`Russell` for `x''`, from there it's immediate by assumption. We
perform the substitution with `chyp-subst`. This takes a direction to
substitute, the hypothesis to use, and another targeting telling us
where to apply the substitution.

``` jonprl
    {
      unfold <not implies>; intro;
      aux { ... };

      assert [Russell ∈ U{i}] <russell-wf>;
      aux { ... };

      assert [(Russell ∈ Russell) ∈ U{i}] <russell-in-russell-wf>;
      aux { ... };

      assert [¬ (Russell ∈ Russell)] <not-russell-in-russell>;
      aux {
        unfold <not implies>;
        intro @i; aux {assumption};
        assert [(R : Russell) * R ~ Russell] <R-with-prop>;
        aux {
          intro [Russell] @i; auto
        };

        elim <R-with-prop>; thin <R-with-prop>;
        unfold <Russell>; elim #5;

        assert [¬ member(Russell; Russell)];
        aux {
          unfold <Russell>;
          chyp-subst ← #8 [h. ¬ (h ∈ h)];
        };
      }
    }
```

This leaves us with a much more tractable goal.

    [main]
    1. x : member(U{i}; U{i})
    2. russell-wf : member({x:U{i} | not(member(x; x))}; U{i})
    3. russell-in-russell-wf : member(member({x:U{i} | not(member(x; x))}; {x:U{i} | not(member(x; x))}); U{i})
    4. x' : member({x:U{i} | not(member(x; x))}; {x:U{i} | not(member(x; x))})
    5. s : {x:U{i} | not(member(x; x))}
    6. x'' : U{i}
    7. [t'] : not(member(x''; x''))
    8. t : ceq(x''; {x:U{i} | not(member(x; x))})
    ⊢ not(member(x''; x''))

    [main]
    1. x : member(U{i}; U{i})
    2. russell-wf : member({x:U{i} | not(member(x; x))}; U{i})
    3. russell-in-russell-wf : member(member({x:U{i} | not(member(x; x))}; {x:U{i} | not(member(x; x))}); U{i})
    4. x' : member({x:U{i} | not(member(x; x))}; {x:U{i} | not(member(x; x))})
    5. s : {x:U{i} | not(member(x; x))}
    6. x'' : U{i}
    7. [t'] : not(member(x''; x''))
    8. t : ceq(x''; {x:U{i} | not(member(x; x))})
    9. H : not(member(Russell; Russell))
    ⊢ void

    [main]
    1. x : member(U{i}; U{i})
    2. russell-wf : member(Russell; U{i})
    3. russell-in-russell-wf : member(member(Russell; Russell); U{i})
    4. russell-not-in-russell : not(member(Russell; Russell))
    ⊢ void

We'd like to imply just apply `assumption` but it's not immediately
applicable due to some technically details (basically we can only
apply an assumption in a proof irrelevant context but we have to
unfold `Russell` and introduce it to demonstrate that it's
irrelevant). So just read what's left as a (very) convoluted
`assumption`.

``` jonprl
    {
      unfold <not implies>; intro;
      aux { ... };

      assert [Russell ∈ U{i}] <russell-wf>;
      aux { ... };

      assert [(Russell ∈ Russell) ∈ U{i}] <russell-in-russell-wf>;
      aux { ... };

      assert [¬ (Russell ∈ Russell)] <not-russell-in-russell>;
      aux {
        unfold <not implies>;
        intro @i; aux {assumption};
        assert [(R : Russell) * R ~ Russell] <R-with-prop>;
        aux {
          intro [Russell] @i; auto
        };

        elim <R-with-prop>; thin <R-with-prop>;
        unfold <Russell>; elim #5;

        assert [¬ (Russell; Russell)];
        aux {
          unfold <Russell>;
          chyp-subst ← #8 [h. ¬ (h ∈ h)];
          unfold <not implies>
          intro; aux { impredicativity-wf-tac };
          contradiction
        };
      }
    }
```

Now we're almost through this assertion, our subgoals look like this
(pay attention to 9 and 4)

    [main]
    1. x : member(U{i}; U{i})
    2. russell-wf : member({x:U{i} | not(member(x; x))}; U{i})
    3. russell-in-russell-wf : member(member({x:U{i} | not(member(x; x))}; {x:U{i} | not(member(x; x))}); U{i})
    4. x' : member({x:U{i} | not(member(x; x))}; {x:U{i} | not(member(x; x))})
    5. s : {x:U{i} | not(member(x; x))}
    6. x'' : U{i}
    7. [t'] : not(member(x''; x''))
    8. t : ceq(x''; {x:U{i} | not(member(x; x))})
    9. H : not(member(Russell; Russell))
    ⊢ void

    [main]
    1. x : member(U{i}; U{i})
    2. russell-wf : member(Russell; U{i})
    3. russell-in-russell-wf : member(member(Russell; Russell); U{i})
    4. russell-not-in-russell : not(member(Russell; Russell))
    ⊢ void

Once we unfold that `Russell` we have an immediate contradiction so
`unfold <Russell>; contradiction` solves it.

``` jonprl
    {
      unfold <not implies>; intro;
      aux { ... };

      assert [Russell ∈ U{i}] <russell-wf>;
      aux { ... };

      assert [(Russell ∈ Russell) ∈ U{i}] <russell-in-russell-wf>;
      aux { ... };

      assert [¬ (Russell ∈ Russell)] <not-russell-in-russell>;
      aux {
        unfold <not implies>;
        intro @i; aux {assumption};
        assert [(R : Russell) * R ~ Russell] <R-with-prop>;
        aux {
          intro [Russell] @i; auto
        };

        elim <R-with-prop>; thin <R-with-prop>;
        unfold <Russell>; elim #5;

        assert [¬ (Russell; Russell)];
        aux {
          unfold <Russell>;
          chyp-subst ← #8 [h. ¬ (h ∈ h)];
          unfold <not implies>;
          intro; aux { impredicativity-wf-tac };
          contradiction
        };

        unfold <Russell>; contradiction
      }
    }
```

This takes care of this subgoal so now we're back on the main
goal. This time though we have an extra hypothesis which will provide
the leverage we need to prove our next assertion.

    [main]
    1. x : member(U{i}; U{i})
    2. russell-wf : member(Russell; U{i})
    3. russell-in-russell-wf : member(member(Russell; Russell); U{i})
    4. russell-not-in-russell : not(member(Russell; Russell))
    ⊢ void

Now we're going to claim that `Russell` is in fact in `Russell`. This
will follow from the fact that we've proved already that `Russell`
isn't in `Russell` (yeah, it seems pretty paradoxical already).

``` jonprl
    {
      unfold <not implies>; intro;
      aux { ... };

      assert [Russell ∈ U{i}] <russell-wf>;
      aux { ... };

      assert [(Russell ∈ Russell) ∈ U{i}] <russell-in-russell-wf>;
      aux { ... };

      assert [¬ (Russell ∈ Russell)] <not-russell-in-russell>;
      aux { ... };

      assert [Russell ∈ Russell];
   }
```

Giving us

    [aux]
    1. x : member(U{i}; U{i})
    2. russell-wf : member(Russell; U{i})
    3. russell-in-russell-wf : member(member(Russell; Russell); U{i})
    4. russell-not-in-russell : not(member(Russell; Russell))
    ⊢ member(Russell; Russell)

    [main]
    1. x : member(U{i}; U{i})
    2. russell-wf : member(Russell; U{i})
    3. russell-in-russell-wf : member(member(Russell; Russell); U{i})
    4. russell-not-in-russell : not(member(Russell; Russell))
    5. H : member(Russell; Russell)
    ⊢ void


Proving this is pretty straightforward, we only have to demonstrate
that `not(Russell ∈ Russell)` and `Russell ∈ U{i}`, both of which we
have as assumptions. The rest of the proof is just more
well-formedness goals.

First we unfold everything and apply `eq-cd`. This gives us 3
subgoals, the first two are `Russell ∈ U{i}` and `¬(Russell ∈
Russell)`. Since we have these as assumptions we'll use `main
{assumption}`. That will target both these goals and prove them
immediately. Here by using `main` we avoid applying this to the
well-formedness goal, which in this case actually *isn't* the
assumption.

``` jonprl
    {
      unfold <not implies>; intro
      aux { ... };

      assert [Russell ∈ U{i}] <russell-wf>;
      aux { ... };

      assert [(Russell ∈ Russell) ∈ U{i}] <russell-in-russell-wf>;
      aux { ... };

      assert [¬ (Russell ∈ Russell)] <not-russell-in-russell>;
      aux { ... };

      assert [Russell ∈ Russell];
      aux {
        unfold <member Russell>; eq-cd;
        unfold <member>;

        main { assumption };
      };
    }
```

This just leaves us with one awful well-formedness goal requiring us
to prove that `not(=(x; x; x))` is a type if `x` is a type. We
actually proved something similar back when we prove that `Russell`
was well formed. The proof is the same as then, just unfold, `eq-cd`
and `impredicativity-wf-tac`. We use `?{!{auto}}` to only apply `auto`
in a subgoal where it immediately proves it. Here `?{}` says "run this
or do nothing" and `!{}` says "run this, if it succeeds stop, if it
does anything else, fail". This is not an interesting portion of the
proof, don't burn too many cycles trying to figure this out.

``` jonprl
    {
      unfold <not implies>; intro
      aux { ... };

      assert [Russell ∈ U{i}] <russell-wf>;
      aux { ... };

      assert [(Russell ∈ Russell) ∈ U{i}] <russell-in-russell-wf>;
      aux { ... };

      assert [¬ (Russell ∈ Russell)] <not-russell-in-russell>;
      aux { ... };

      assert [Russell ∈ Russell] <russell-in-russell>;
      aux {
        unfold <member Russell>; eq-cd;
        unfold <member>;

        main { assumption };
        unfold <not implies>; eq-cd; ?{!{auto}};
        impredicativity-wf-tac;
      };
    }
```

Now we just have the final subgoal to prove. We're actually in a
position to do so now.

    [main]
    1. x : member(U{i}; U{i})
    2. russell-wf : member(Russell; U{i})
    3. russell-in-russell-wf : member(member(Russell; Russell); U{i})
    4. russell-not-in-russell : not(member(Russell; Russell))
    5. russell-in-russell : member(Russell; Russell)
    ⊢ void

Now that we've shown `P` and `not(P)` hold at the same time all we
need to do is apply `contradiction` and we're done.


``` jonprl
    Theorem type-not-in-type [¬ (U{i} ∈ U{i})] {
      unfold <not implies>; intro
      aux { ... };

      assert [Russell ∈ U{i}] <russell-wf>;
      aux { ... };

      assert [(Russell ∈ Russell) ∈ U{i}] <russell-in-russell-wf>;
      aux { ... };

      assert [¬ (Russell ∈ Russell)] <not-russell-in-russell>;
      aux { ... };

      assert [Russell ∈ Russell] <russell-in-russell>;
      aux { ... };

      contradiction
    }.
```

And there you have it, a complete proof of Russell's paradox fully
formalized in JonPRL! We actually proved a slightly stronger result
than just that the type of types cannot be in itself, we proved that
at any point in the hierarchy of universes (the first of which is
`Type`/`*`/whatever) if you tie it off, you'll get a contradiction.

## Wrap Up

I hope you found this proof interesting. Even if you're not at all
interested in JonPRL, it's nice to see that allowing one to have
`U{i} : U{i}` or `* :: *` gives you the ability to have a type like
`Russell` and with it, `void`. I also find it especially pleasing that
we can prove something like this in JonPRL, it's growing up so fast.

*Thanks to Jon for greatly improving the original proof we had*

[dep-haskell]: https://typesandkinds.wordpress.com/category/dependent-types/haskell-dependent-types/
[jonprl]: http://github.com/jonsterling/jonprl
