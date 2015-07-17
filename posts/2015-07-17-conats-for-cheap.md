---
title: Coinduction in JonPRL for Low Low Prices
tags: jonprl
---

So as a follow up to my prior [tutorial on JonPRL][tutorial] I wanted
to demonstrate a nice example of JonPRL being used to prove something

 1. Interesting
 2. Unreasonably difficult in Agda or the like

    *I think I'm asking to be shown up when I say stuff like this...*

I would like to implement the conatural numbers in JonPRL but without
a notion of general coinductive *or* even inductive types. Just the
natural numbers. The fun bit is that we're basically going to lift the
definition of a coinductively defined set straight out of set theory
into JonPRL!

## Math Stuff

First, let's go through some math. How can we formalize the notion of
an coinductively defined type as we're used to in programming
languages? Recall that something is coinductively if it contains all
terms so that we can eliminate the term according to the elimination
form for our type. For example, Martin-Lof has proposed we view
functions (Π-types) as coinductively defined. That is,

    x : A ⊢ f(x) : B(x)
    ————————————————————
     f : Π x : A. B(x)

In particular, there's no assertion that `f` needs to be a lambda,
just that `f(x)` is defined and belongs to the right type. This view
of "if we can use it, it's in the type" applies to more than just
functions. Let's suppose we have a type with the following elimination
form

    L : List  M : A  x : Nat, y : List : A
    ——————————————————————————————————————
          case(L; M; x.y.N) : A

This is more familiar to Haskellers as

    case L of
      [] -> M
      x :: y -> N

Now if we look at the coinductively defined type built from this
elimination rule we have not finite lists, but streams! There's
nothing in this elimination rule that specifies that the list be
finite in length for it to terminate. All we need to be able to do is
evaluate the term to either a `::` of a `Nat` and a `List` or
`nil`. This means that

    fix x. cons(0; x) : List

Let's now try to formalize this by describing what it means to build a
coinductively type up as a set of terms. In particular the types we're
interested in here are algebraic ones, constructed from sums and
products.


Now unfortunately I'm going to be a little handwavy. I'm going to
act is if we've worked out a careful set theoretic semantics for this
programming language (like the one that exists for MLTT). This means
that All the equations you see here are across sets and that these
sets contain programs so that `⊢ e : τ` means that `e ∈ τ` where `τ`
on the right is a set.

Well we start with some equation of the form

    Φ = 1 + Φ

This particular equation a is actually how we would go about
defining the natural numbers. If I write it in a more Haskellish
notation we'd have

    data Φ = Zero | Succ Φ

Next, we transform this into a function. This step is a
deliberate move so we can start applying the myriad tools we know
of for handling this equation.

    Φ(X) = 1 + X

We now want to find some `X` so that `Φ(X) = X`. If we can do this,
then I claim that `X` is a solution to the equation given above since

    X = Φ(X)
    X = 1 + X

precisely mirrors the equation we had above. Such an `X` is called a
"fixed point" of the function `Φ`. However, there's a catch: there may
well be more than one fixed point of a function! Which one do we
choose? The key is that we want the coinductively defined
version. Coinduction means that we should always be able to examine a
term in our type and its outermost form should be `1 + ???`. Okay,
let's optimistically start by saying that `X` is `⊤` (the collection
of all terms).

Ah okay, this isn't right. This works only so long as we don't make
any observations about a term we claim is in this type. The minute we
pattern match, we might have found we claimed a function was in our
type! I have not yet managed to pay my rent by saying "OK, here's the
check... but don't try to *use* it and it's rent". So perhaps we should
try something else. Okay, so let's not say `⊤`, let's say

    X = ⊤ ⋂ Φ(⊤)

Now, if `t ∈ X`, we know that `t ∈ 1 + ???`. This means that if
we run `e ∈ X`, we'll get the correct outermost form. However, this
code is still potentially broken:

``` haskell
    case e of
      Inl _ -> ...
      Inr x -> case e of
                 Inl _ -> ...
                 Inr _ -> ...
```

This starts off as being well typed, but as we evaluate, it may
actually become ill typed. If we claimed that this was a fixed point
to our language, our language would be type-unsafe. This is an
unappealing quality in a type theory.

Okay, so that didn't work. What if we fixed this code by doing

    X = ⊤ ⋂ Φ(⊤) ⋂ Φ(Φ(⊤))

Now this fixes the above code, but can you imagine a snippet of code
where this still gets stuck? So each time we intersect `X` with `Φ(X)`
we get a new type which behaves like the real fixed point so long as
we only observe `n + 1` times where `X` behaves like the fixed point
for `n` observations. Well, we can only make finitely many
observations so let's just iterate such an intersection

    X = ⋂ₙ Φⁿ(⊤)

So if `e ∈ X`, then no matter how many times we pattern match and
examine the recursive component of `e` we know that it's still in
`⋂ₙ Φⁿ(⊤)` and therefore still in `X`! In fact, it's easy to prove
that this is the case with two lemmas

 1. If `X ⊆ Y` then `Φ(X) ⊆ Φ(Y)`
 2. If I have a collection `S` of sets, then `⋂ Φ(S) = Φ(⋂ S)` where
    we define `Φ` on a collection of sets by applying `Φ` to each
    component.

These two properties state the monotonicity and cocontinuity of
`Φ`. In fact, cocontinuity should imply monotonicity (can you see
how?). We then may show that

    Φ(⋂ₙ Φⁿ(⊤)) = ⋂ₙ Φ(Φⁿ(⊤))
                 = ⊤ ⋂ (⋂ₙ Φ(Φⁿ(⊤)))
                 = ⋂ₙ Φⁿ(⊤)

As desired.

## The Code

Now that we have some idea of how to formalize coinduction, can we port
this to JonPRL? Well, we have natural numbers and we can take the
intersection of types... Seems like a start. Looking at that example, we
first need to figure out what `⊤` corresponds to. It should include
all programs, which sounds like the type `base` in JonPRL. However, it
also should be the case that `x = y ∈ ⊤` for all `x` and `y`. For that
we need an interesting trick:

``` jonprl
    Operator top : ().
    [top] =def= [isect(void; _.void)].
```

In prettier notation,

    top ≙ ⋂ x : void. void

Now `x ∈ top` if `x ∈ void` for all `_ ∈ void`. Hey wait a minute... No
such `_` exists so the if is always satisfied vacuously. Ok, that's
good. Now `x = y ∈ top` if for all `_ ∈ void`, `x = y ∈ void`. Since
no such `_` exists again, all things are in fact equal in `void`. We can
even prove this within JonPRL

``` jonprl
    Theorem top-is-top :
      [isect(base; x.
       isect(base; y.
       =(x; y; top)))] {
      unfold <top>; auto
    }.
```

This proof is really just:

 1. Unfold all the definitions.
 2. Hey! There's a `x : void` in my context! Tell me more about that.

Now the fact that `x ∈ top` is a trivial corollary since our theorem
tells us that `x = x ∈ top` and the former is just sugar for the
latter. With this defined, we can now write down a general operator
for coinduction!

``` jonprl
    Operator corec : (1).
    [corec(F)] =def= [isect(nat; n. natrec(n; top; _.x. so_apply(F;x)))].
```

To unpack this, `corec` takes one argument which binds one
variable. We then intersect the type `natrec(n; top;
_.x.so_apply(F;x))` for all `n ∈ nat`. That `natrec` construct is
really saying `Fⁿ(⊤)`, it's just a little obscured. Especially since
we have to use `so_apply`, a sort of "meta-application" which lets us
apply a term binding a variable to another term. This should look
familiar, it's just how we defined fixed point of a `Φ`!

For a fun demo, let's define an `F` so that `cofix(F)` will give us
the conatural numbers. I know that the natural numbers come from the
least fixed point of `X ↦ 1 + X` (because I said so above, so it must
be so) so let's define that.

``` jonprl
    Operator conatF : (0).
    [conatF(X)] =def= [+(unit; X)].
```

This is just that `X ↦ 1 + X` I wrote above in JonPRL land instead of
math notation. Next we need to actually define conatural numbers using
`corec`.

``` jonprl
    Operator conat : ().
    [conat] =def= [corec(R. conatF(R))].
```

Now I've defined this, but that's no fun unless we can actual build
some terms so that `member(X; conat)`. Specifically I want to prove
two things to start

 1. `member(czero; conat)`
 2. `fun(member(M; conat); _.member(csucc(M); conat))`

This states that `conat` is closed under some zero and successor
operations. Now what should those operations be? Remember what I said
before, that we had this correspondence?

    X    ↦   1   +   X
    Nat     Zero   Suc X

Now remember that `conat` is `isect(nat; n....)` and when constructing a
member of `isect` we're not allowed to mention that `n` in it (as
opposed to `fun` where we do exactly that). So that means `czero` has
to be a member of `top` and `sum(unit; ...)`. The `top` bit is easy,
everything is in `top`! That diagram above suggests `inl` of
*something* in unit

``` jonprl
    Operator czero : ().
    [czero] =def= [inl(<>)].
```

So now we want to prove that this in fact in `conat`.

``` jonprl
    Theorem zero-wf : [member(czero; conat)] {

    }.
```

Okay loading this into JonPRL gives

    ⊢ czero ∈ conat

From there we start by unfolding all the definitions

``` jonprl
    {
       unfold <czero conat conatF corec top>
    }
```

This gives us back the desugared goal

    ⊢ inl(<>) ∈ ⋂n ∈ nat. natrec(n; top; _.x.+(unit; x))

Next let's apply all the obvious introductions so that we're in a
position to try to prove things

``` jonprl
    unfold <czero conat conatF corec top>; auto
```

This gives us back

    1. [n] : nat
    ⊢ inl(<>) = inl(<>) ∈ natrec(n; top; _.x.+(unit; x))

Now we're stuck. We want to show `inl` is in something, but we're
never going to be able to do that until we can reduce that
`natrec(n; top; _.x.+(unit; x))` to a canonical form. Since it's stuck
on `n`, let's induct on that `n`. After that, let's immediately
reduce.

``` jonprl
    {
      unfold <czero conat conatF corec top>; auto; elim #1; reduce
    }
```

Now we have to cases, the base and inductive case.

    1. [n] : nat
    ⊢ inl(<>) = inl(<>) ∈ top


    1. [n] : nat
    2. n' : nat
    3. ih : inl(<>) = inl(<>) ∈ natrec(n'; top; _.x.+(unit; x))
    ⊢ inl(<>) = inl(<>) ∈ +(unit; natrec(n'; top; _.x.+(unit; x)))

Now that we have canonical terms on the right of the `∈`m, let's let
`auto` handle the rest.

``` jonprl
    Theorem zero-wf : [member(czero; conat)] {
      unfold <czero conat conatF corec top>; auto; elim #1; reduce; auto
    }.
```

So now we have proven that `czero` is in the correct type. Let's
figure out `csucc`? Going by our noses, `inl` corresponded to `czero`
and our diagram says that `inr` should correspond to `csucc`. This
gives us

``` jonprl
    Operator csucc : (0).
    [csucc(M)] =def= [inr(M)].
```

Now let's try to prove the corresponding theorem for `csucc`

``` jonprl
    Theorem succ-wf : [isect(conat; x. member(csucc(x); conat))] {
    }.
```

Now we're going to start off this proof like we did with our last
one. Unfold everything, apply the introduction rules, and induct on
`n`.

``` jonprl
    {
      unfold <csucc conat conatF corec top>; auto; elim #2; reduce
    }
```

Like before, we now have two subgoals:

    1. [x] : ⋂n ∈ nat. natrec(n; ⋂_ ∈ void. void; _.x.+(unit; x))
    2. [n] : nat
    ⊢ inr(x) = inr(x) ∈ ⋂_ ∈ void. void


    1. [x] : ⋂n ∈ nat. natrec(n; ⋂_ ∈ void. void; _.x.+(unit; x))
    2. [n] : nat
    3. n' : nat
    4. ih : inr(x) = inr(x) ∈ natrec(n'; ⋂_ ∈ void. void; _.x.+(unit; x))
    ⊢ inr(x) = inr(x) ∈ +(unit; natrec(n'; ⋂_ ∈ void. void; _.x.+(unit; x)))

The first one looks pretty easy, that's just `foo ∈ top`, I think
`auto` should handle that.

``` jonprl
    {
      unfold <csucc conat conatF corec top>; auto; elim #2; reduce;
      auto
    }
```

This just leaves one goal to prove

    1. [x] : ⋂n ∈ nat. natrec(n; ⋂_ ∈ void. void; _.x.+(unit; x))
    2. [n] : nat
    3. n' : nat
    4. ih : inr(x) = inr(x) ∈ natrec(n'; ⋂_ ∈ void. void; _.x.+(unit; x))
    ⊢ x = x ∈ natrec(n'; ⋂_ ∈ void. void; _.x.+(unit; x))

Now, as it turns out, this is nice and easy: look at what our first
assumption says! Since `x ∈ isect(nat; n.Foo)` and our goal is to
show that `x ∈ Foo(n')` this should be as easy as another call to
`elim`.

``` jonprl
    {
      unfold <csucc conat conatF corec top>; auto; elim #2; reduce;
      auto; elim #1 [n']; auto
    }
```

Note that the `[n']` bit there lets us supply the term we wish to
substitute for `n` while eliminating. This leaves us here:

    1. [x] : ⋂n ∈ nat. natrec(n; ⋂_ ∈ void. void; _.x.+(unit; x))
    2. [n] : nat
    3. n' : nat
    4. ih : inr(x) = inr(x) ∈ natrec(n'; ⋂_ ∈ void. void; _.x.+(unit; x))
    5. y : natrec(n'; ⋂_ ∈ void. void; _.x.+(unit; x))
    6. z : y = x ∈ natrec(n'; ⋂_ ∈ void. void; _.x.+(unit; x))
    ⊢ x = x ∈ natrec(n'; ⋂_ ∈ void. void; _.x.+(unit; x))

Now a small hiccup: we know that `y = x` is in the right type. so `x = x`
in the right type. But how do we prove this? The answer is to
substitute all occurrences of `x` for `y`. This is written

``` jonprl
    {
      unfold <csucc conat conatF corec top>; auto; elim #2; reduce;
      auto; elim #1 [n']; auto;
      hyp-subst ← #6 [h.=(h; h; natrec(n'; isect(void; _.void); _.x.+(unit;x)))];
    }
```

There are three arguments here, a direction to substitute, an index
telling us which hypothesis to use as the equality to substitute with
and finally, a term `[h. ...]`. The idea with this term is that each
occurrence of `h` tells us where we want to substitute. In our case we
used `h` in two places: both where we use `x`, and the direction says
to replace the right hand side of the equality with the left side of
the equality.

Actually running this gives

    1. [x] : ⋂n ∈ nat. natrec(n; ⋂_ ∈ void. void; _.x.+(unit; x))
    2. [n] : nat
    3. n' : nat
    4. ih : inr(x) = inr(x) ∈ natrec(n'; ⋂_ ∈ void. void; _.x.+(unit; x))
    5. y : natrec(n'; ⋂_ ∈ void. void; _.x.+(unit; x))
    6. z : y = x ∈ natrec(n'; ⋂_ ∈ void. void; _.x.+(unit; x))
    ⊢ y = y ∈ natrec(n'; ⋂_ ∈ void. void; _.x.+(unit; x))


    1. [x] : ⋂n ∈ nat. natrec(n; ⋂_ ∈ void. void; _.x.+(unit; x))
    2. [n] : nat
    3. n' : nat
    4. ih : inr(x) = inr(x) ∈ natrec(n'; ⋂_ ∈ void. void; _.x.+(unit; x))
    5. y : natrec(n'; ⋂_ ∈ void. void; _.x.+(unit; x))
    6. z : y = x ∈ natrec(n'; ⋂_ ∈ void. void; _.x.+(unit; x))
    7. h : natrec(n'; ⋂_ ∈ void. void; _.x.+(unit; x))
    ⊢ h = h ∈ natrec(n'; ⋂_ ∈ void. void; _.x.+(unit; x)) ∈ U{i}

The first goal is the result of our substitution and it's trivial;
`auto` will handle this now. The second goal is a little strange. It
basically says that the result of our substitution is still a
well-formed type. JonPRL's thought process is something like this

> You said you were substituting for things of this type
> here. However, I know that just because `x : A` doesn't mean we're
> using it in all those spots as if it has type `A`. What if you
> substitute things equal in top (always equal) for when they're being
> used as functions! This would let us prove that `zero ∈ Π(...)` or
> something silly. Convince me that when we fill in those holes with
> *something* of the type you mentioned, the goal is still a type (in a
> universe).

However, these well-formedness goals usually go away with auto. This
completes our theorem in fact.

``` jonprl
    Theorem succ-wf : [isect(conat; x. member(csucc(x); conat))] {
      unfold <csucc conat conatF corec top>; auto; elim #2; reduce;
      auto; elim #1 [n']; auto;
      hyp-subst ← #6 [h.=(h; h; natrec(n'; isect(void; _.void); _.x.+(unit;x)))];
      auto
    }.
```

Okay so we now have something kind of number-ish, with zero and
successor. But in order to demonstrate that this is the conatural
numbers there's one big piece missing.

## The Clincher

The thing that distinguishes the *co*natural numbers from the
inductive variety is the fact that we include infinite terms. In
particular, I want to show that Ω (infinitely many `csucc`s) belongs
in our type.

In order to say Ω in JonPRL we need recursion. Specifically, we want to
write

``` jonprl
    [omega] =def= [csucc(omega)].
```

But this doesn't work! Instead, we'll define the Y combinator and say

``` jonprl
    Operator omega : ().
    [omega] =def= [Y(x.csucc(x))].
```

So what should this `Y` be? Well the standard definition of Y is

    Y(F) = (λ x. F (x x)) (λ x. F (x x))

Excitingly, we can just say that in JonPRL; remember that we have a
full untyped computation system after all!

``` jonprl
    Operator Y : (1).
    [Y(f)] =def= [ap(lam(x.so_apply(f;ap(x;x)));lam(x.so_apply(f;ap(x;x))))].
```

This is more or less a direct translation, we occasionally use
`so_apply` for the reasons I explained above. As a fun thing, try to
prove that this is a fixed point, eg that

``` jonprl
    isect(U{i}; A. isect(fun(A; _.A); f . ceq(Y(f); ap(f; Y(f)))))
```

The complete proof is in the JonPRL repo under
`example/computational-equality.jonprl`. Anyways, we now want to prove

``` jonprl
    Theorem omega-wf : [member(omega; conat)] {

    }.
```

Let's start with the same prelude

``` jonprl
    {
      *{unfold <csucc conat conatF corec top omega Y>}; auto; elim #1;
    }
```

Two goals just like before

    1. [n] : nat
    ⊢ (λx. inr(x[x]))[λx. inr(x[x])] = (λx. inr(x[x]))[λx. inr(x[x])] ∈ natrec(zero; ⋂_ ∈ void. void; _.x.+(unit; x))


    1. [n] : nat
    2. n' : nat
    3. ih : (λx. inr(x[x]))[λx. inr(x[x])] = (λx. inr(x[x]))[λx. inr(x[x])] ∈ natrec(n'; ⋂_ ∈ void. void; _.x.+(unit; x))
    ⊢ (λx. inr(x[x]))[λx. inr(x[x])] = (λx. inr(x[x]))[λx. inr(x[x])] ∈ natrec(succ(n'); ⋂_ ∈ void. void; _.x.+(unit; x))

The goals start to get fun now. I've also carefully avoided using
`reduce` ike we did before. The reason is simple, if we reduce in the
second goal, our `ih` will reduce as well and we'll end up completely
stuck in a few steps (try it and see). So instead we're going to
finesse it a bit.

First let's take care of that first goal. We can tell JonPRL to apply
some tactics to just the first goal with the `focus` tactic

``` jonprl
    {
      *{unfold <csucc conat conatF corec top omega Y>}; auto; elim #1;
      focus 0 #{reduce 1; auto};
    }
```

Here `reduce 1` says "reduce by only one step" since really omega will
diverge if we just let it run. This takes care of the first goal
leaving just

    1. [n] : nat
    2. n' : nat
    3. ih : (λx. inr(x[x]))[λx. inr(x[x])] = (λx. inr(x[x]))[λx. inr(x[x])] ∈ natrec(n'; ⋂_ ∈ void. void; _.x.+(unit; x))
    ⊢ (λx. inr(x[x]))[λx. inr(x[x])] = (λx. inr(x[x]))[λx. inr(x[x])] ∈ natrec(succ(n'); ⋂_ ∈ void. void; _.x.+(unit; x))

Here's the proof sketch for what's left

 1. Reduce the goal by one step but *carefully avoid touching the ih*
 2. Step everything by one
 3. The result follows by intro and assumption

You can stop here or you can see how we actually do this. It's
somewhat tricky. The basic complication is that there's no built-in
tactic for 1. Instead we use a new type called `ceq` which is
"computational equality". It ranges between two terms, no types
involved here. It's designed to work thusly if `ceq(a; b)`, either

  1. `a` and `b` run to weak-head normal form (canonical
     verifications) with the same outermost form, and all the inner
     operands are `ceq`
  2. `a` and `b` both diverge

So if `ceq(a; b)` then `a` and `b` "run the same". What's a really
cool upshot of this is that if `ceq(a; b)` then if `a = a ∈ A` and `b
= b ∈ A` then `a = b ∈ A`! `ceq` is the strictest equality in our
system and we can rewrite with it absolutely everywhere without
regards to types. Proving this requires showing the above definition
forms a congruence (two things are related if their subcomponents are
related).

This was a *big deal* because until Doug Howe came up with this proof
NuPRL/CTT was awash with rules trying to specify this idea chunk by
chunk and showing those rules were valid wasn't trivial. Actually, you
should [read that paper][howe]: it's 6 pages and the proof concept
comes up a lot.

So, in order to do 1. we're going to say "the goal and the goal if we
step it twice are computationally equal" and then use this fact to
substitute for the stepped version. The tactic to use here is called
`csubst`. It takes two arguments

 1. The `ceq` we're asserting
 2. Another `h.` term to guide the rewrite

``` jonprl
    {
      *{unfold <csucc conat conatF corec top omega Y>}; auto; elim #1;
      focus 0 #{reduce 1; auto};
      csubst [ceq(ap(lam(x.inr(ap(x;x))); lam(x.inr(ap(x;x))));
                  inr(ap(lam(x.inr(ap(x;x))); lam(x.inr(ap(x;x))))))]
         [h.=(h;h; natrec(succ(n'); isect(void; _. void); _.x.+(unit; x)))];
    }
```

This leaves us with two goals

    1. [n] : nat
    2. n' : nat
    3. ih : (λx. inr(x[x]))[λx. inr(x[x])] = (λx. inr(x[x]))[λx. inr(x[x])] ∈ natrec(n'; ⋂_ ∈ void. void; _.x.+(unit; x))
    ⊢ ceq((λx. inr(x[x]))[λx. inr(x[x])]; inr((λx. inr(x[x]))[λx. inr(x[x])]))


    1. [n] : nat
    2. n' : nat
    3. ih : (λx. inr(x[x]))[λx. inr(x[x])] = (λx. inr(x[x]))[λx. inr(x[x])] ∈ natrec(n'; ⋂_ ∈ void. void; _.x.+(unit; x))
    ⊢ inr((λx. inr(x[x]))[λx. inr(x[x])]) = inr((λx. inr(x[x]))[λx. inr(x[x])]) ∈ natrec(succ(n'); ⋂_ ∈ void. void; _.x.+(unit; x))

Now we have two goals. The first is that `ceq` proof obligation. The
second is our goal post-substitution. The first one can easily be
dispatched by `step`. `step` let's us prove `ceq` by saying

  1. `ceq(a; b)` holds if
  2. `a` steps to `a'` in one step
  3. `ceq(a'; b)`

This will leave us with `ceq(X; X)` which `auto` can handle. The
second term is.. massive. But also simple. We just need to step it
once and we suddenly have `inr(X) = inr(X) ∈ sum(_; A)` where
`X = X ∈ A` is our assumption! So that can also be handled by `auto`
as well. That means we need to run `step` on the first goal,
`reduce 1` on the second, and `auto` on both.

``` jonprl
    Theorem omega-wf : [member(omega; conat)] {
      unfolds; unfold <omega Y>; auto; elim #1;
      focus 0 #{reduce 1; auto};
      csubst [ceq(ap(lam(x.inr(ap(x;x))); lam(x.inr(ap(x;x))));
                  inr(ap(lam(x.inr(ap(x;x))); lam(x.inr(ap(x;x))))))]
             [h.=(h;h; natrec(succ(n'); isect(void; _. void); _.x.+(unit; x)))];
      [step, reduce 1]; auto
    }.
```

And we've just proved that `omega ∈ conat`, a term that is certainly
the canonical (heh) example of coinduction in my mind.

## Wrap Up

Whew, I actually meant for this to be a short blog post but that
didn't work out so well. Hopefully this illustrated a cool trick in
computer science (intersect your way to coinduction) and in
JonPRL.

Funnily enough before this was written no one had actually realized
you could do coinduction in JonPRL. I'm still somewhat taken with the
fact that a *very* minimal proof assistant like JonPRL is powerful
enough to let you do this by giving you such general purpose tools as
family intersection and a full computation system to work with. Okay
that's enough marketing from me.

Cheers.

*Huge thanks to Jon Sterling for the idea on how to write this code and
then touching up the results*

[tutorial]: /posts/2015-07-06-jonprl.html
[howe]: http://www.nuprl.org/documents/Howe/EqualityinLazy.html
