---
title: Notes on Proof Theory: Part 1
tags: types
---

I write a lot about types. Up until now however, I've only made
passing references to the thing I've actually been studying in most of
my free time lately: proof theory. Now I have a good reason for this:
the proof theory I'm interested in is undeniably intertwined with type
theory and computer science as a whole. In fact, you occasionally see
someone draw the triangle


               Type Theory
              /           \
             /             \
     Proof Theory ---- Category Theory

Which nicely summarizes the lay of the land in the world I'm
interested in. People will often pick up something will understood on
one corner of the triangle and drag it off to another, producing a
flurry of new ideas and papers. It's all very exciting and leads to
really cool stuff. I think the most talked about example lately is
homotopy type theory which drags a mathematical structure (weak
infinite groupoids) and hoists off to type theory!

If you read the [unprofessional, mostly incorrect, and entirely more
fun to read] blog posts on these subjects you'll find most of the lip
service is paid to category theory and type theory with poor proof
theory shunted off to the side.

In this post, I'd like to jot down my notes on Frank Pfenning's
introduction to proof theory materials to change that in some small
way.

## What is Proof Theory

The obvious question is just "What is proof theory?". The answer is
that proof theory is the study of proofs. In this world we study
proofs as first class mathematical objects which we prove interesting
things about. This is the branch of math that formalizes our handwavy
notion of a proof into a precise object governed by rules.

We can then prove things like "Given a proof that `Γ ⊢ A` and another
derivation of `Γ, A ⊢ B`, then we can produce a derivation of
`Γ ⊢ B`. Such a theorem is utterly crazy unless we can formalize what
it means to derive something.

From this we grow beautiful little sets of rules and construct
derivations with them. Later, we can drag these derivations off to
type theory and use them to model all sorts of wonderful phenomena. My
most recent personal example was when folks noticed that the rules for
modal logic perfectly capture what the semantics of static pointers
ought to be.

So in short, proof theory is devoted to answering that question that
every single one of your math classes dodged

> Professor, what exactly is a proof?

## Basic Building Blocks

In every logic that we'll study we'll keep circling back to two core
objects: judgments and propositions. The best explanation of judgments
I've read comes from Frank Pfenning

> A judgment is something we may know, that is, an object of
> knowledge. A judgment is evident if we in fact know it.

So judgments are the things we'll structure our logic around. You've
definitely heard of one judgment: `A true`. This judgment signifies
whether or not some proposition `A` is true. Judgments can be much
fancier though: we might have a whole bunch of judgments like
`n even`, `A possible` or `A resource`.

These judgments act across various syntactic objects. In particular,
from our point of view we'll understand the meaning of a proposition
by the ways we can prove it, that is the proofs that `A true` is
evident.

We prove a judgment `J` through inference rules. An inference rule
takes the form

    J₁ J₂ .... Jₓ
    —————————————
         J

Which should be read as "When `J₁`, `J₂` ... and `Jₓ` hold, then so
does `J`". Here the things above the line are premises and the ones
below are conclusions. What we'll do is define a bunch of these
inference rules and use them to construct proofs of judgments. For
example, we might have the inference rules

                 n even
     ——————    ————————————
     0 even    S(S(n)) even

for the judgment `n even`. We can then form proofs to show that `n
even` holds for some particular `n`.


           ——————
           0 even
        ————————————
        S(S(0)) even
     ——————————————————
     S(S(S(S(0)))) even

This tree for example is evidence that `4 even` holds. We apply
second inference rule to `S(S(S(S(0))))` first. This leaves us with
one premise to show, `S(S(0)) even`. For this we repeat the process
and end up with the new premise that `0 even`. For this we can apply
the first inference rule which has no premises completing our proof.

One judgment we'll often see is `A prop`. It simply says that `A` is a
well formed proposition, not necessarily true but syntactically well
formed. This judgment is defined inductively over the structure of
`A`. An example judgment would be

    A prop  B prop
    ——————————————
      A ∧ B prop

Which says that `A ∧ B` (A and B) is a well formed proposition if and
only if `A` and `B` are! We can imagine a whole bunch of these rules

                    A prop B prop
    ——————  ——————  ————————————— ...
    ⊤ prop  ⊥ prop    A ∨ B prop

that lay out the propositions of our logic. This doesn't yet tell us
how prove any of these propositions to be true, but it's a
start. After we formally specify what sentences are propositions in
our logic we need to discuss how to prove that one is true. We do this
with a different judgment `A true` which is once again defined
inductively.

For example, we might want to give meaning to the proposition `A ∧ B`.
To do this we define its meaning through the inference rules for
proving that `A ∧ B true`. In this case, we have the rule

    A true  B true
    —————————————— (∧ I)
      A ∧ B true

I claim that this defines the meaning of `∧`: to prove a conjunction
to be true we must prove its left and right halves. The rather
proof-theoretic thing we've done here is said that the meaning of
something is what we use to prove it. This is sometimes called the
"verificationist perspective". Finally, note that I annotated this
rule with the name `∧ I` simply for convenience to refer it.

Now that we know what `A ∧ B` means, what does have a proof of it
imply? Well we should be able to "get out what we put in" which would
mean we'd have two inference rules

    A ∧ B true    A ∧ B true
    ——————————    ——————————
      A true        B true

We'll refer to these rules as `∧ E1` and `∧ E2` respectively.

Now for a bit of terminology, rules that let us "introduce" new proofs
of propositions are introduction rules. Once we have a proof, we can
use it to construct other proofs. The rules for how we do that are
called elimination rules. That's why I've been adding I's and E's to
the ends of our rule names.

How do we convince ourselves that these rules are correct with
respect to our understanding of `∧`? This question leads us to our
first sort of proofs-about-proofs we'll make.

## Local Soundness and Completeness

What we want to say is that the introduction and elimination rules
match up. This should mean that anytime we prove something with an by
an introduction rule followed by an elimination rule, we should be
able to rewrite to avoid this duplication. This also hints that the
rules aren't too powerful: we can't prove anything with the
elimination rules that we didn't have a proof for at some point
already.

For `∧` this proof looks like this

      D  E
      –  –
      A  B            D
     —————— ∧I   ⇒  ————
      A ∧ B           A
     —————— ∧E 1
        A

So whenever we introduce a ∧ and then eliminate it with `∧ E1` we can
always rewrite our proof to not use the elimination rules. Here notice
that D and E range over *derivations* in this proof. They represent a
chain of rule applications that let us produce an `A` or `B` in the
end. Note I got a bit lazy and started omitting the `true` judgments,
this is something I'll do a lot since it's mostly unambiguous.

The proof for `∧E2` is similar.

      D  E
      –  –
      A  B            E
      ————— ∧I   ⇒  ————
      A ∧ B           B
      ————— ∧E 2
        B

Given this we say that the elimination rules for ∧ are "locally
sound". That is, when used immediately after an elimination rule they
don't let us produce anything truly new.

Next we want to show that if we have a proof of `A ∧ B`, the
elimination rules give us enough information that we can pick the proof
apart and produce a reassembled `A ∧ B`.


               D           D
             ————–       ————–
      D      A ∧ B       A ∧ B
    ————— ⇒ —————∧E1   ——————∧E2
    A ∧ B      A           B
             ———————————————— ∧I
                   A ∧ B

This somewhat confusion derivation takes our original proof of `A ∧ B`
and pulls it apart into proof of `A` and `B` and uses these to
assemble a new proof of `A ∧ B`. This means that our elimination rules
give us all the information we put in so we say their locally
complete.

The two of these properties combined, local soundness and completeness
are how we show that an elimination rule is balanced with its
introduction rule.

If you're more comfortable with programming languages (I am) our local
soundness property is equivalent to stating that

    fst (a, b) ≡ a
    snd (a, b) ≡ b

And local completeness is that

    a ≡ (fst a, snd a)

The first equations are reductions and the second is expansion. These
actually correspond the eta and beta rules we expect a programming
language to have! This is a nice little example of why proof theory is
useful, it gives a systematic way to define some parts of the behavior
of a program. Given the logic a programming language gives rise to we
can double check that all rules are locally sound and complete which
gives us confidence our language isn't horribly broken.

## Hypothetical Judgments

Before I wrap up this post I wanted to talk about one last important
concept in proof theory: judgments with hypotheses. This is best
illustrated by trying to write the introduction and elimination rules
for "implies" or "entailment", written `A ⊃ B`.

Clearly `A ⊃ B` is supposed to mean we can prove `B true` assume `A true` to
be provable. In other words, we can construct a derivation of the form

     A true
     ——————
       .
       .
       .
     ——————
     B true

We can notate our rules then as


     —————— u
     A true
     ——————
       .
       .
       .
     ——————
     B true           A ⊃ B    A
     —————————— u     ——————————
     A ⊃ B true         B true

This notation is a bit clunky, so we'll opt for a new one: `Γ ⊢ J`. In
this notation `Γ` is some list of judgments we assume to hold and `J`
is the thing we want to show holds. Generally we'll end up with the rule

    J ∈ Γ
    —————
    Γ ⊢ J

Which captures the fact that Γ contains assumptions we may or may not
use to prove our goal. This specific rule may vary depending on how we
want express how assumptions work in our logic (substructural logics
spring to mind here). For our purposes, this is the most
straightforward characterization of how this ought to work.

Our hypothetical judgments come with a few rules which we call
"structural rules". They modify the structure of judgment, rather than
any particular proposition we're trying to prove.


    Weakening
      Γ ⊢ J
    —————————
    Γ, Γ' ⊢ J

    Contraction
    Γ, A, A, Γ' ⊢ J
    ———————————————
     Γ, A, Γ' ⊢ J

    Exchange
    Γ' = permute(Γ)   Γ' ⊢ A
    ————————————————————————
            Γ ⊢ A

Finally, we get a substitution principle. This allows us to eliminate
some of the assumptions we made to prove a theorem.

    Γ ⊢ A   Γ, A ⊢ B
    ————————————————
         Γ ⊢ B

These 5 rules define meaning to our hypothetical judgments. We can
restate our formulation of entailment with less clunky notation then
as

    A prop  B prop
    ——————————————
      A ⊃ B prop

    Γ, A ⊢ B      Γ ⊢ A ⊃ B    Γ ⊢ A
    —————————     ——————————————————
    Γ ⊢ A ⊃ B           Γ ⊢ B

One thing in particular to note here is that entailment actually
internalizes the notion of hypothetical judgments into our logic. This
the aspect of it that made it behave so differently then the other
connectives we looked at.

As an exercise to the reader: prove the local soundness and
completeness of these rules.

## Conclusion

In this post we've layed out a bunch of rules and I've hinted that a
bunch more are possible. When put together these rules define a logic
using "natural deduction", a particular way of specifying proofs that
uses inference rules rather than axioms or something entirely
different.

Hopefully I've inspired you to poke a bit further into proof theory,
in that case I heartily recommend
[Frank Pfenning's lectures][lectures] at the Oregon Summer School for
Programming Languages.

Cheers,

[lectures]: https://www.cs.uoregon.edu/research/summerschool/summer12/curriculum.html
