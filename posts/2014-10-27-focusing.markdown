---
title: Notes on Focusing
tags: types
---

I've been spending a lot of time whacking my head on focusing
literature. I'd like to jot down some intuition around what a focused
system is and how it relates to the rest of the world. I'm going to
steer clear of actually proving things but I will point out where a
proof would be needed.

### What Is Focusing

In a nutshell, focusing is a strategy to create proofs that minimizes
the amount of choices available at each step. Focusing is thus
amenable to mechanization since a computer is very good at applying a
lot of deterministic procedures but incredibly bad at nondeterministic
choice.

Now when we set out to define a focused system we usually do something like

 1. Formalize our logical framework with natural deduction
 2. Translate our framework into a sequent calculus
 3. Transform our sequent calculus into a focused variant

At each of these steps there's a proof that says something like
"System 2 is sound and complete with respect to System 1". We can then
chain these proofs together to get that we can transform any
nonfocused proof into a focused one (focalization) and the reverse
(de-focalization).

In order to actually carry out these proofs there's a fair amount of
work and pain. Usually we'll need something like cut elimination
and/or identity expansion.

### Groundwork

Now before we go on to define an example logic, let's notice a few
things about logical. First off, in sequent calculus there are left
and right rules. Left rules decompose known facts into other known
facts while right rules transform our goal. There's also an identity
sequent which more or less just states

     A is an atom
     —————————————
       Γ, A → A

This is a bit boring though, so we'll circle back to it later.

Now certain rules are invertible: their conclusion implies their
premise in addition to the reverse. For example if I said you must
prove `A ∧ B` clearly we'll have to prove both `A` and `B` in order to
prove `A ∧ B`; there's no alternative set of rule applications that
let us circumvent proving `A` and `B`.

This means that if we were mechanically trying to prove something of
the form `A ∧ B` we can immediately apply the right rule that
decomposes `∧` into 2 goals.

We can these sort of rules invertible or asynchronous. Dually, there
are rules that when applied transform our goal into something
impossible to prove. Consider `⊥ ∨ ⊤`, clearly apply the rule that
transforms this into `⊥` would be a bad idea!

Now if we begin classifying all the left and write rules we'll notice
that the tend to all into 2 categories

 - Things with invertible left rules and noninvertible right rules
 - Things with noninvertible left rules and invertible right rules

We dub the first group "positive" things and the second "negative"
things. This is called polarization and isn't strictly necessary but
greatly simplifies a lot of our system.

Now there are a few things that could be considered both positive and
negative. For example we can consider `∧` as positive with

      Γ → A⁺  Γ → B⁺
     ———————————————
       Γ → A⁺ ∧ B⁺

       Γ, A⁺, B⁺ → C
     —————————————————
       Γ, A⁺ ∧ B⁺ → C

In this case, the key determiner for the polarity of ∧ comes from its
subcomponents. We can just treat ∧ as positive along with its
subcomponents and with an appropriate dual ∧⁻, our proof system will
still be complete.

As a quick example, implication `⊂` is negative. the right rule

     Γ, A → B
    ——————————
    Γ → A ⊃ B

While its left rule isn't

     Γ, A ⊃ B → A  Γ, B, A ⊃ B → C
     ——————————————————————————————
             Γ, A ⊃ B → C

Since we could easily have something like `⊥ ⊃ ⊤` but using this rule
would entail (heh) proving `⊥`! Urk. If our system applied this rules
remorselessly, we'd quickly end up in a divergent proof search.x

### An Actual Focused System

Now that we've actually seen some examples of invertible rules and
polarized connectives, let's see how this all fits into a coherent
system. There is one critical change we must make to the sttructure of
our judgments: an addition to the form `_ → _`. Instead of just an
unordered multiset on the left, in order to properly do inversion we
change this to `Γ; Ω → A` where Ω is an ordered list of propositions
we intend to focus on. More on this momentarily.

Furthermore, since we're dealing with a polarized calculus, we
occasionally want to view positive things as negative and vice
versa. For this we have shifts, ↓ and ↑. When we're focusing on some
proposition and we reach a shift, we pop out of the focused portion of
our judgment.

Our system is broken up into 3 essentially separate judgments. In this
judgment we basically apply as many invertible rules as many places as
we can.

     Γ, A⁻; Q ⊢ U
    ——————————————
    Γ; ↓A⁻, Q ⊢ U

    Γ; A⁺, Ω ⊢ U  Γ; B+; Ω ⊢ U
    ———————————————————————————
        Γ; A⁺ ∨ B⁺, Ω ⊢ U

      Γ; A⁺, B⁺, Ω ⊢ U
    ————————————————————
      Γ; A⁺ ∧ B⁺, Ω ⊢ U

    ——————————————
     Γ; ⊥, Ω ⊢ U



We first look at how to break down Ω into simpler forms. The idea is
that we're going to keep doing there's nothing left in Ω. Ω can only
contain positive propositions so eventually we'll decompose everything
to shifts (which we move into Γ) ⊤+ (which we just drop on the floor)
or ⊥ (which means we're done). These are all invertible rules to we
can safely apply them eagerly and we won't change the provability of
our goal.

Once we've moved everything out of Ω we can make a choice. If `U` is
"stable" meaning that we can't break it down further easily, we can
pick a something negative out of our context and focus on it

       Γ; [A⁻] ⊢ U
      ————————————–
      Γ, A⁻; • ⊢ U

This pops us into the next judgment in our calculus. However, if U is
not stable, then we have to decompose it further as well.

      Γ; • ⊢ A⁺
    ——————————————
      Γ; • ⊢ ↑ A⁺

    ———————————
     Γ; • ⊢ ⊤⁻

      Γ; A⁺ ⊢ B⁻
    —————————————
    Γ; • ⊢ A⁺ ⊃ B⁻

    Γ; • ⊢ A⁻   Γ; • ⊢ B⁻
    —————————————————————
       Γ; • ⊢ A⁻ ∧ B⁻

If we have a negative connective at the top level we can decompose
that further, leaving us with a strictly smaller goal. Finally, we may
reach a positive proposition with nothing in Ω. In this case we focus
on the right.

      Γ ⊢ [A⁺]
    ———————————
     Γ; • ⊢ A⁺

Now we're in a position to discuss these two focused judgments. If we
focus on the right we decompose positive connectives

    ——————————
     Γ ⊢ [⊤⁺]

    Γ; • ⊢ A⁻
    —————————
    Γ ⊢ ↓ A⁻

       Γ ⊢ [A⁺]
    —————————————
     Γ ⊢ [A⁺ ∨ B⁺]

       Γ ⊢ [B⁺]
    —————————————
     Γ ⊢ [A⁺ ∨ B⁺]

    Γ ⊢ [A⁺]   Γ ⊢ [B⁺]
    ———————————————————
       Γ ⊢ [A⁺ ∧ B⁺]

These judgments follow the ones we've already seen. If we encounter a
shift, we stop focusing. Otherwise we decompose the topmost positive
connective. Now looking at these, you should see that sometimes these
rules we'll lead us to a "mistake". Imagine if we applied the 4th rule
to `⊤ ∨ ⊥`! This is why these rules are segregated into a separate
judgment.

In this judgment's dual we essentially apply the exact same rules to
the left of the turnstile and on negative connectives.

      Γ; A⁺ ⊢ U
    ————————————
    Γ; [↑A⁺] ⊢ U

    Γ ⊢ [A⁺]   Γ; [B⁻] ⊢ U
    ——————————————————————
      Γ; [A⁺ ⊃ B⁻] ⊢ U

       Γ; [A⁻] ⊢ U
    —————————————————
     Γ; [A⁻ ∧ B⁻] ⊢ U

       Γ; [B⁻] ⊢ U
    —————————————————
     Γ; [A⁻ ∧ B⁻] ⊢ U

That wraps up our focused system. The idea is now we have this much
more limited system which can express the same things our original,
unfocused system could. A computer can be easily programmed to do a
focused search since there's much less backtracking everywhere leading
to fewer rules being applicable at each step. I think Pfenning has
referred to this as removing most of the "don't-care" nondeterminism
from our rules.

### Wrap Up

I'm going to wrap up the post here. Proving focalization or even
something like cut elimination is quite fiddly and I have no desire
at all to try to transcribe it (painfully) into markdown and get it
wrong in the process.

Instead, now that you have some idea of what focusing is about, go
read Rob Simmons' [paper][paper]. It provides a clear account of
proving everything necessary prove a focused system is complete and
sound with respect to its unfocused counterpart.

Cheers

[paper]: http://www.cs.cmu.edu/~rjsimmon/drafts/focus.pdf
