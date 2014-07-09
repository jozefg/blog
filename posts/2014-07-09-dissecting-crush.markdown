---
title: Dissecting crush
---

For almost a year and half now I've been referencing one particular
book on Coq,
[Certified Programming with Dependent Types][cpdt-website]. CPDT is a
literate program on building practical things with Coq.

One of the main ideas of CPDT is that proofs ought to be fully
automated. This means that a proof should be primarily a logic program
(Ltac) which constructs some boring and large proof term. To this end,
CPDT has a bunch of Ltac "tactics" for constructing such logic
programs.

Since CPDT is a program, there's actual working source for each of
these tactics. It occurred to me today that in my 18 months of
blinking uncomprehendingly at CPDT, I've never read its source for
these tactics.

In this post, we'll dissect how CPDT's main tactic for automation,
`crush`, actually works. In the process, we'll get the chance to explore
some nice, compositional, ltac engineering as well as a whole host
of useful tricks.

## The Code

The first step to figuring out of `crush` works is actually finding
where it's defined.

After downloading the source to CPDT I ran

    grep "Ltac crush :=" -r .

And found in `src/CpdtTactics`, line 205

    Ltac crush := crush' false fail.

Glancing at `crush'`, I've noticed that it pulls in almost every
tactic in `CpdtTactics`. Therefore, we'll start at the top of this
file and work our way done, dissecting each tactic as we go.

Incidentally, since CpdtTactics is an independent file, if you're confused about
something firing up your coq dev environment of choice and trying things out with
`Goal` inline works nicely.

Starting from the top, our first tactic is `inject`.

    Ltac inject H := injection H; clear H; intros; try subst.

This is just a quick wrapper around `injection`, which also does the normal
operations one wants after calling `injection`. It clears the original hypothesis and
brings our new equalities into our environment so future tactics can use them. It also tries
to swap out any variables with our new equalities using `subst`. Notice the `try` wrapper
since `subst` is one of those few tactics that will fail if it can't do anything useful.

Next up is

    Ltac appHyps f :=
      match goal with
        | [ H : _ |- _ ] => f H
      end.

`appHyps` makes use of the backtracking nature of `match goal with`. It'll
apply `f` to every hypothesis in the current environment and stop once it find
a hypothesis `f` works with.

Now we get to some combinators for working with hypothesis.

    Ltac inList x ls :=
      match ls with
        | x => idtac
        | (_, x) => idtac
        | (?LS, _) => inList x LS
      end.

`inList` takes a faux-list of hypothesis and looks for an occurrence of a particular
lemma `x`. When it finds it we just run `idtac` which does nothing. In the case were
we can't match `x` anywhere, `inList` will just fail with the standard "No matching
clause" message.

Next we have the equivalent of `appHyps` for tupled lists

    Ltac app f ls :=
      match ls with
        | (?LS, ?X) => f X || app f LS || fail 1
        | _ => f ls
      end.

This works exactly like `appHyps` but instead of looking through the proofs
environment, we're looking through `ls`. It has the same "keep the first
result that works" semantics too. One thing that confused me was the
`_ => f ls` clause of this tactic. Remember that with our tupled lists we don't
have a "nil" member. But rather the equivalent of

    A :: B :: C :: Nil

is

    ((A, B), C)

So when we don't have a pair, `ls` itself is the last hypothesis in our list. As
a corollary of this, there is no obvious "empty" tupled list, only one with a useless
last hypothesis.

Next we have `all`, which runs `f` on *every* member in `f ls`.

    Ltac all f ls :=
      match ls with
        | (?LS, ?X) => f X; all f LS
        | (_, _) => fail 1
        | _ => f ls
      end.

Careful readers will notice that instead of `f X || ...` we use `;`. Additionally,
if the first clause fails and the second clause matches, that means that either `f X`
or `all f LS` failed. In this case we backtrack all the way back out of this clause. This
should mean that this is a "all or nothing" tactic. It will either not fail on all
members of `ls` or nothing at all will happen.

Now we get to the first *big* tactic

    Ltac simplHyp invOne :=
      let invert H F :=
        inList F invOne;
          (inversion H; fail)
          || (inversion H; [idtac]; clear H; try subst) in
    
      match goal with
        | [ H : ex _ |- _ ] => destruct H
        | [ H : ?F ?X = ?F ?Y |- ?G ] =>
          (assert (X = Y); [ assumption | fail 1 ])
          || (injection H;
            match goal with
              | [ |- X = Y -> G ] =>
                try clear H; intros; try subst
            end)
        | [ H : ?F ?X ?U = ?F ?Y ?V |- ?G ] =>
          (assert (X = Y); [ assumption
            | assert (U = V); [ assumption | fail 1 ] ])
          || (injection H;
            match goal with
              | [ |- U = V -> X = Y -> G ] =>
                try clear H; intros; try subst
            end)
    
        | [ H : ?F _ |- _ ] => invert H F
        | [ H : ?F _ _ |- _ ] => invert H F
        | [ H : ?F _ _ _ |- _ ] => invert H F
        | [ H : ?F _ _ _ _ |- _ ] => invert H F
        | [ H : ?F _ _ _ _ _ |- _ ] => invert H F
    
        | [ H : existT _ ?T _ = existT _ ?T _ |- _ ] => generalize (inj_pair2 _ _ _ _ _ H); clear H
        | [ H : existT _ _ _ = existT _ _ _ |- _ ] => inversion H; clear H
        | [ H : Some _ = Some _ |- _ ] => injection H; clear H
      end.


Wow, just a little bit bigger than what we've been working with so far.

The first small chunk of `simpleHyp` is a tactic for doing clever inversion using
the tuple list `invOne`.

     invert H F :=
       inList F invOne;
       (inversion H; fail)
         || (inversion H; [idtac]; clear H; try subst)

Here `H` is a hypothesis that we're thinking about inverting on and `F` is the head symbol of
`H`. First we run the `inList` predicate, meaning that we don't invert upon anything
that we don't want to. If the head symbol of `H` is something worth inverting upon we try
two different types of inversion.

In the first case `inversion H; fail` we're just looking
for an "easy proof" where inverting `H` immediately dispatches the current goal. In the second
case `inversion H; [idtac]; clear H; try subst`, we invert upon `H` iff it only generates 1
subgoal. Remember that `[t | t' | t'']` is a tactic that runs `t` on the first subgoal, t' on
the second, and so on. If the number of goals don't match, `[]` will fail. So `[idtac]` is just
a clever way of saying "there's only one new subgoal". Next we get rid of the hypothesis we just
inverted on (it's not useful now, and we don't want to try inverting it again) and see if any
substitutions are applicable.

Alright! Now let's talk about the massive `match goal with` going on in `simplHyp`.

The first branch is

        | [ H : ex _ |- _ ] => destruct H

This just looks for a hypothesis with an existential (remember that
`ex` is what `exists` desugars to). If we find one, we introduce a new
variable to our environment and instantiate H with it. The fact that this
doesn't recursively call `simplHyp` probably means that we want to do something
like `repeat simplHyp` to ensure this is applied everywhere.

Next we look at simplifying hypothesis where injection applies. There are two almost
identical branches, one for constructors of two parameters, one for one. Let's look at
the latter since it's slightly simpler.

        | [ H : ?F ?X = ?F ?Y |- ?G ] =>
          (assert (X = Y); [ assumption | fail 1 ])
          || (injection H;
            match goal with
              | [ |- X = Y -> G ] =>
                try clear H; intros; try subst
            end)


This looks for an equality over a constructor `F`. This branch is looking to prove that
`X = Y`, a fact deducible from the injectiveness of F.

The way that we go about doing this
is actually quite a clever ltac trick though. First we assert `X = Y`, this will generate to
subgoals, the first that `X = Y` (shocker) and the second is the current goal `G`, with the
new hypothesis that `X = Y`. We attempt to prove that `X = Y` by `assumption`. If this works,
than we already trivially can deduce `X = Y` so there's no point in doing all that `injection`
stuff so we `fail 1` and bomb out of the whole branch.

If `assumption` fails we'll jump to the other side of the `||`s and actually use `injection`.
We only run `injection` if it generates a proof that `X = Y` in which case we do the normal
cleanup with trying to clear our original fact and do some substitution.

The next part is fairly straightforward, we make use of that `invert` tactic and run it over
facts we have floating around in our environment 

        | [ H : ?F _ |- _ ] => invert H F
        | [ H : ?F _ _ |- _ ] => invert H F
        | [ H : ?F _ _ _ |- _ ] => invert H F
        | [ H : ?F _ _ _ _ |- _ ] => invert H F
        | [ H : ?F _ _ _ _ _ |- _ ] => invert H F

Notice that we can now use the match to grab the leading symbol for `H` so we only invert
upon hypothesis that we think will be useful.

Next comes a bit of axiom-fu

        | [ H : existT _ ?T _ = existT _ ?T _ |- _ ] =>
            generalize (inj_pair2 _ _ _ _ _ H); clear H

`inj_pair2` is function that lives in the Coq standard library and has the type

    forall (U : Type) (P : U -> Type) (p : U) (x y : P p),
           existT P p x = existT P p y -> x = y

This relies on `eq_rect_eq` so it's just a little bit dodgy for something like HoTT where
we give more rope to `=` than just `refl`.

This particular branch
of the match is quite straightforward though. Once we see an equality between two witnesses
for the same existential type, we just `generalize` the equality between their proofs into
our goal.


If this fails however, we'll fall back to standard inversion with


        | [ H : existT _ _ _ = existT _ _ _ |- _ ] => inversion H; clear H


Finally, we have one last special case branch for `Some`. This is because
the branches above will fail when phased with a polymorphic constructor

        | [ H : Some _ = Some _ |- _ ] => injection H; clear H

Nothing exciting going on there.

So that wraps up `simplHyp`. It's just a conglomeration of useful stuff to do
to constructors in our hypothesis.

Onwards we go! Next is a simple tactic for automatically rewriting with
a hypothesis

    Ltac rewriteHyp :=
      match goal with
        | [ H : _ |- _ ] => rewrite H by solve [ auto ]
      end.

like most of the other tactics we saw earlier, this will hunt for an `H`
where this works and then stop. The `by solve [auto]` will run `solve [auto]`
against all the hypothesis that the `rewrite` generates and ensure that
`auto` solves all the new goals. This prevents a rewrite from going and introducing
obviously false facts as goals for a rewrite that made no sense.

We can combine this with `autorewrite` with two simple tactics

    Ltac rewriterP := repeat (rewriteHyp; autorewrite with core in *).
    Ltac rewriter := autorewrite with core in *; rewriterP.

This just repeatedly rewrite with `autorewrite` and `rewriteHyp` as
long as they can. Worth noticing here how we can use `repeat` to make
these smaller tactics modify *all* applicable hypothesis rather than just
one.

Next up is an innocent looking definition that frightens me a little bit

    Definition done (T : Type) (x : T) := True.

What frightens me about this is that Adam calls this "devious".. and when
*he* calls something clever or devious I'm fairly certain I'd never
be able to come up with it :)

What this actually appears to do is provide a simple way to "stick" something
into an environment. We can trivially prove `done T x` for any `T` and `x` but
having this in an environment also gives us a proposition `T` and a ready
made proof of it `x`! This is useful for tactics since we can do something like

    assert (done SomethingUseful usefulPrf) by constructor

and viola! Global state without hurting anything.

We use these in the next tactic, `instr`.

    Ltac inster e trace :=
      match type of e with
        | forall x : _, _ =>
          match goal with
            | [ H : _ |- _ ] =>
              inster (e H) (trace, H)
            | _ => fail 2
          end
        | _ =>
          match trace with
            | (_, _) =>
              match goal with
                | [ H : done (trace, _) |- _ ] =>
                  fail 1
                | _ =>
                  let T := type of e in
                    match type of T with
                      | Prop =>
                        generalize e; intro;
                          assert (done (trace, tt)) by constructor
                      | _ =>
                        all ltac:(fun X =>
                          match goal with
                            | [ H : done (_, X) |- _ ] => fail 1
                            | _ => idtac
                          end) trace;
                        let i := fresh "i" in (pose (i := e);
                          assert (done (trace, i)) by constructor)
                    end
              end
          end
      end.

Another big one!

This match is a little different than the previous ones. It's not a match goal
but a `match type of ... with`. This is used to examine one particular hypothesis'
type and match over that.

This particular `match` has two branches. The first deals with the case
where we have uninstantiated universally quantified variables.

     | forall x : _, _ =>
        match goal with
          | [ H : _ |- _ ] =>
            inster (e H) (trace, H)
          | _ => fail 2
        end

If our hypothesis does, we randomly grab a hypothesis, instantiate `e` with it,
add `H` to the trace list, and then recurse.

If there isn't a hypothesis, then we fail out of the toplevel match and exit the
tactic.

Now the next branch is where the real work happens

      | _ =>
        match trace with
          | (_, _) =>
            match goal with
              | [ H : done (trace, _) |- _ ] =>
                fail 1
              | _ =>
                let T := type of e in
                  match type of T with
                    | Prop =>
                      generalize e; intro;
                        assert (done (trace, tt)) by constructor
                    | _ =>
                      all ltac:(fun X =>
                        match goal with
                          | [ H : done (_, X) |- _ ] => fail 1
                          | _ => idtac
                        end) trace;
                      let i := fresh "i" in (pose (i := e);
                        assert (done (trace, i)) by constructor)
                  end
             end
          end

We first chekc to make sure that `trace` isn't empty. If this is the case, then
we know that we instantiated `e` with at least *something*. If we have, we snoop
around to see if there's a `done` in our environment with the same trace. If this
is the case, we know that we've done an identical instantiation of `e` before hand
so we backtrack to try another one.

Otherwise, we look to see what `e` was instantiated too. If it was a simple `Prop`,
we just stick a `done` record of this instantiation into our environment and
add our new instantiated `e` back in with `generalize`. If `e` isn't a proof,
we do the same thing. In this case, however, we must also double check that
the things we used to instantiate `e` with aren't results of `inster` as well otherwise
our combination of backtracking/instantiating can lead to an infinite loop.

Since this tactic generates a bunch of `done`'s that are otherwise useless, a tactic
to clear them is helpful.

    Ltac un_done :=
      repeat match goal with
               | [ H : done _ |- _ ] => clear H
             end.

Hopefully by this point this isn't too confusing. All this tactic does is
loop through the environment and clear all `done`s.

Now, finally, we've reached `crush'`.

    Ltac crush' lemmas invOne :=
      let sintuition := simpl in *; intuition; try subst;
        repeat (simplHyp invOne; intuition; try subst); try congruence in
    
      let rewriter := autorewrite with core in *;
        repeat (match goal with
                  | [ H : ?P |- _ ] =>
                    match P with
                      | context[JMeq] => fail 1
                      | _ => rewrite H by crush' lemmas invOne
                    end
                end; autorewrite with core in *) in
    
        (sintuition; rewriter;
          match lemmas with
            | false => idtac            | _ =>
              (** Try a loop of instantiating lemmas... *)
              repeat ((app ltac:(fun L => inster L L) lemmas
              (** ...or instantiating hypotheses... *)
                || appHyps ltac:(fun L => inster L L));
              (** ...and then simplifying hypotheses. *)
              repeat (simplHyp invOne; intuition)); un_done
          end;
          sintuition; rewriter; sintuition;
          try omega; try (elimtype False; omega)).

`crush'` is really broken into 3 main
components.

First is a simple tactic `sintuition`

    sintuition := simpl in *; intuition; try subst;
        repeat (simplHyp invOne; intuition; try subst); try congruence

So this first runs the normal set of "generally useful tactics" and then
breaks out some of first custom tactics. This essentially will act
like a souped-up version of `intuition` and solve goals that are trivially
solvable with straightforward inversions and reductions.

Next there's a more powerful version of `rewriter`

    rewriter := autorewrite with core in *;
        repeat (match goal with
                  | [ H : ?P |- _ ] =>
                    match P with
                      | context[JMeq] => fail 1
                      | _ => rewrite H by crush' lemmas invOne
                    end
                end; autorewrite with core in *)

This is almost identical to what we have above but instead of solving
side conditions with `solve [auto]`, we use `crush'` to hopefully
deal with a larger number of possible rewrites.

Finally, we have the main loop of `crush'`.

    (sintuition; rewriter;
      match lemmas with
        | false => idtac
        | _ =>
          repeat ((app ltac:(fun L => inster L L) lemmas
            || appHyps ltac:(fun L => inster L L));
          repeat (simplHyp invOne; intuition)); un_done
      end;
      sintuition; rewriter; sintuition;
    try omega; try (elimtype False; omega)).

Here we run the `sintuition` and `rewriter` and then get
to work with the lemmas we supplied in `lemmas`.

The first branch is just a match on `false`, which we
use like a nil. Since we have no hypothesis we don't do
anything new.

If we do have lemmas, we try instantiating both
them and our hypothesis as many times as necessary  and
then repeatedly simplify the results. This loop will ensure that
we make full use of bot our supplied lemmas and the surrounding environment.

Finally, we make another few passes with `rewriter` and `sintuition` attempting
to dispatch our goal using our new, instantiated and simplified environment.

As a final bonus, if we *still* haven't dispatched our goal, we'll run `omega`
to attempt to solve a Presburger arithmetic. On the off chance that we have
something `omega` can be contradictory, we also try `elimType false; omega` to
try to exploit such a contradiction.

So all `crush` does is call this tactic with no lemmas (`false`) and no suggestions
to invert upon (`fail`). There you have it, and it only took 500 lines to get here.

## Wrap Up

So that's it, hopefully you got a few useful Ltac trick out of reading this. I certainly
did writing it :)

If you enjoyed these tactics, there's a more open-source version of these
tactics, on the [CPDT website][cpdt-website]. It might also interest you to read the rest
of `CpdtTactics.v` since it has some useful gems like `dep_destruct`.

Last but not least, if you haven't read CPDT itself and you've made it this far, go
read it! It's available as either dead-tree or online. I still reference it regularly
so I at least find it useful. It's certainly better written than this post :)

<i>Note, all the code I've shown in this post is from CPDT and is licensed under
ANCND license. I've removed some comments from the code where they wouldn't
render nicely with them.</i>

[cpdt-website]: http://adam.chlipala.net/cpdt/
