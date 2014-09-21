---
title: What Are Dependent Types
---

It seems like dependent types are becoming "popular". Well, as close
to popular as something so staunchly academic gets.. In any case, 
I'd rather like to discuss exactly *what* these dependent types are.

Since I write quite a lot of Haskell, this post is mostly aimed at a
Haskeller interested in some more fancy type systems.

## Quick Note on Notation

Since I'm aiming this post at Haskellers, it's worth taking a moment
to clarify some of the notation I'll use in this post.

The most familiar is `a : T`, which should be read as "`a` has the
type `T`".

Sometimes, we which to indicate that `a` has the type `T` in some
particular scope or environment. An environment is just a list of type
assertions, `a : T, b : U, c : V`. This environment is traditionally
called Γ. So to say that `a : T` in some environment `Γ`, we'd write

    Γ ⊢ a : T

Sometimes these type assertions have
preconditions. For example, `f x : B` when `f : A → B` and `x : A`.

We'd write this as

    Γ ⊢ f : A → B,  Γ ⊢ x : A
    —————————————————————————–
       Γ ⊢ f x : B

Finally, we can indicate substituting a variable for a value in an
expression. Substituting `val` for `var` in `e` is `[val/var]e`.

That should cover it as var as notation goes. If something is
unfamiliar to you, please comment so I can add it to this section.

Now, let's get back to the subject at hand.

## What is a Dependent Type

So the most obvious question to ask is what is a dependent type? Then
unhelpful answer is that a dependent type is one which "depends" on
a value.

This is best explained through example. Let's first talk about
functions. Now the basic type of a function is just

    A → B

and these are governed by a rule something like

    Γ ⊢ x : A, Γ ⊢ f : A → B
    ——————————————————————————
         Γ ⊢ f x : B

now if we allowed our function types to be dependent, then the
result *type* could depend on the input *value*. This is a bit of a
departure from the norm. A function's result value always depends
on the input, but normally the type is fixed, static.

Let's notate the dependent function type (Pi types) like this

    Π (x : A) B

where `B` is some expression that may refer to the free variable
`x`. Now the typing rule for function types becomes

    Γ ⊢ a : A, Γ ⊢ f : Π (x : A) B
    ———————————————————————————————–
         Γ ⊢ f a : [a/x]B

Notice the symmetry here, `B` is "applied" to the argument just like the
actual function value!

Coming from Haskell, this seems kinda useless. After all, what could
`B` possibly be to "use" `x`. Well the simplest example is for
something like `head`. We can imagine a type for head along the lines
of

    headTy :: [a] → Type
    headTy []      = ()
    headTy (_ : _) = a

Now `head` could be implemented as


    head :: Π (xs : [a]) (headTy xs)
    head []     = () -- Empty case
    head (x:xs) = x  -- Not empty case

Sweet! So the return type of `head` no varies based on the input. In
particular it's type seems to be defined by some sort of souped up
type family: `headTy`. We'll discuss the implications of these for
now.

Now we can also take an alternative route and let users define their
own dependent types.

To do this we can use something like generalized GADTs (GGADTs :).

Let's give the perennial example, lists indexed with their length:

    data IList (A : Type) : Int → Type where
      Nil  : IList A 0
      Cons : A → IList A n → IList A (n + 1)

Now if we squint at this, we notice that `IList` depends on a
value. The proper way to say this would be to state that `IList` is
indexed by `Int`.

This gives a bit of a hint for how we can actually use these pi types!

    append :: Π (i : Int) Π (j : Int)
            (IList A i → IList A j → IList A (i + J))
    append 0 j [] r              = r
    append (i + 1) j (l : ls) rs = l : append i j ls rs

Here we use the pi types to construct a member of the `IList` type
constructor. Notice that the first and second arguments determine the
type of the third and fourth argument respectively.

These pi types can subsume parametric polymorphism!

    identity : Π (A : Type) (A → A)

We can depend on an argument from the universe of *types* rather than
some particular value, giving us the equivalent of the familiar `∀`
from System F.

Let's now explore one of the most important dependent types: dependent
products!

Now remember that a product (tuple) in a normal language is a type
constructor `×` that takes two arguments. We then have three functions


    product :: A → B → A × B
    fst :: A × B → A
    snd :: A × B → B

Two make this a dependent type, we let `B` depend on the value that we
supply for `A`. This new type is often called `Σ`.

    product :: Π (a : A) (B a → Σ A B)

So now `Σ` still has two arguments, one of type `Type`, and one of
type `A → Type`. Now `fst` is still a fairly simple type

    fst :: Σ A B → A

but `snd` is a bit more complicated, what type should it return? Since
`B : A → Type` it's not immediately obvious. Clearly it's return type
depends on what the first value of the pair is. Dependent types to the
rescue

    snd :: Π (p : Σ A B) (B (fst p))

The type of `snd` depends on the pair we supply it. To determine this
type we extract the first component of the pair and feed it to `B`
constructing the appropriate concrete type.

Some languages also provide large elimination. This is when a type
family pattern matches on a value to determine a type. For example

    isZero :: Int → Type
    isZero 0 = Unit
    isZero _ = Void

This turns out to be critical for a large number of proofs and while
not fundamental most DT languages support this.

Now let's talk about what about what dependent types mean on a more
philosophical level

## Curry Howard

Now, it'd be impossible to avoid talking about the Curry Howard
[faux]-Isomorphism when introducing dependent types. At it's core the
Curry Isomorphism boils down to two key ideas

 - Types in a DT system are isomorphic to logical propositions
 - Values are isomorphic to proofs of their type/proposition

Now what do I mean when I say "proposition"? I mean a logical
proposition, like

    ∀ A. A ∧ ¬ A

These propositions are considered in an constructive logic, this is a
logic without the law of excluded middle. I'm going to skip over a lot
of the finicky details here because this subject really merits its
own post.

Let's go over a few examples of this correspondence. First things
first, implication! In this constructive logic, the following rule
exists for using an implication

      x : A, f : A → B
    ————————————————————
         f x : B

Where `x`, `f`, and `f x` all represent "proof terms". Now does that
look familiar? That's just the inference rule for function types! So
the type constructor `->` is the same as logical implication!

To save a bit of space, I'll just list of the other correspondences

   1. `->` - Implication
   2. `Either` - ∨
   3. `×` - ∧
   4. `Unit` - ⊤
   5. `Void` - ⊥

At some point in the future I'll go over this in more detail.

## A Bigger Example

Now let's go over a slightly larger example of dependent types,
building and verifying a library for natural numbers. To do this I'm
going to switch to Agda syntax, mostly because I'm not smart enough to
do this example with a typechecker :) The main difference is that
`Π (x : A) B` is now `(x : A) → B` and `Type` is called `Set`.

First things first, let's define a type for natural numbers.

    data Nat : Set where
      Zero : Nat
      Succ : Nat → Nat

Now to understand this, mentally translate `Succ x` to `1 + x`. So

    Zero = 0
    Succ Zero = 1
    Succ (Succ Zero) = 2
    ...

Let's define `plus` as a function across these natural numbers

    plus : Nat → Nat → Nat
    plus Zero m     = m
    plus (Succ n) m = Succ (plus n m)
      
We recursively define plus so that `0 + m = m` and
`(1 + n) + m = 1 + (n + m)`. Now we'd like to prove some properties
about `plus` in order to convince ourselves that this definition is
correct. In order to do this we need to find a way to represent `=` as
a type.

For that, we'll define a new dependent type


    data _≡_ {A : Set} (x : A) : A → Set where
        Refl : x ≡ x

This definition says that there is a new type `≡` with 3 arguments,
one type `A` and two values of `A`. The only way to construct an `≡`
is when those to values are the same.

Now we can write a few proofs,

    extend : (n m : Nat) → n ≡ m → Succ n ≡ Succ m
    extend n .n Refl = Refl

This first proof states something obvious, if `n = m` then
`n + 1 = m + 1`, but this fact will come in handy in a moment.

Now we'll set out to prove that zero is an identity for `plus`

    leftId : (n : Nat) → plus Zero n ≡ n
    leftId _ = Refl

    rightId : (n : Nat) → plus n Zero ≡ n
    rightId Zero     = Refl
    rightId (Succ n) = extend (plus n Zero) n (rightId n)

`leftId` went through easily, but we needed to do a bit more work with
`rightId`. This is because our definition of `plus` was defined to
induct upon the left most term so `plus Zero n` trivially reduces to
`n`.

We make a proof by induction in these dependently typed languages with
recursion! This is a common practice: most complex proofs will proceed
by induction on the structure of the argument. To articulate these
proofs we recurse on the argument with strictly smaller terms. The
discussion of what qualifies as "smaller" is a
[rich subject][halting-oracles].

Now let's wrap up this "library" with a classic proof, that
`a + b = b + a`. To do this we'll prove a few quick facts about
equality

    sym : {A : Set}(a b : A) → a ≡ b → b ≡ a
    sym a .a Refl = Refl
    
    trans : {A : Set}(a b c : A) → a ≡ b → b ≡ c → a ≡ c
    trans a .a .a Refl Refl = Refl

These two terms just prove that transitivity and symmetry both hold
for this definition of equality, basic sanity stuff.

Now to the meat of our proof,

    moveSucc : (n m : Nat) →  Succ (plus n m) ≡ plus n (Succ m)
    moveSucc Zero m     = Refl
    moveSucc (Succ n) m = extend (Succ (plus n m)) (plus n (Succ m)) (moveSucc n m)
    
    comm : (n m : Nat) → plus n m ≡ plus m n
    comm Zero m     = sym (plus m Zero) m (rightId m)
    comm (Succ n) m = trans (Succ (plus n m)) (Succ (plus m n)) (plus m (Succ n))
        (extend (plus n m) (plus m n) (comm n m)) (moveSucc m n)

Now this proof is quite ugly, we can clean it up a bit by forcing Agda
to automagically construct some arguments. Some of the arguments for
`trans` and `extend` merely exist to parametrize the next few
arguments. We can force Agda to construct these with `_`.

    comm : (n m : Nat) → plus n m ≡ plus m n
    comm Zero m     = sym _ _ (rightId m)
    comm (Succ n) m = trans _ _ _ (extend _ _ (comm n m)) (moveSucc m n)

Now the fog clears a bit. Our base case is when `n` is `Zero`, this is
just the same as `rightId` except flipped. Next we glue together the
inductive case `extend _ _ (comm n m)` with `moveSucc` to produce the
desired equality.

## Verification

This last section showed that we can write a normal functional program
and then prove facts about its operation with dependent types.
This gives rise to a powerful paradigm of verifying the correctness of
programs, almost like a souped-up version of QuickCheck properties.

Just like QuickCheck properties we don't try to prove something to
magically "right" (What does that mean?) but rather prove that certain
facts hold. Once we know that these certain facts are true we stitch
them together into a strong guarantee of correctness.

This shifts what code we trust. Rather than trusting our potentially
large and complicated program, we instead trust our small and simple
specifications.

For example if we wrote some algorithm to calculate the square root of
a number, we might end up with something like


    sqrt :: Double → Double
    sqrt x = ... magic here ...

Now the logic of `sqrt` is probably very complicated if we're trying
to maximize speed, so complicated that it would be difficult to assure
ourselves of it's correctness just by looking at it.

In a normal language we'd just write some tests!

    assert(sqrt(9)   == 3)
    assert(sqrt(16)  == 4)
    assert(sqrt(100) == 10)

But that's not really helpful, there are always cases we don't cover
and each of them potentially hides subtle bugs. We could try writing a
QuickCheck property

    prop_squareSqrt :: Double → Bool
    prop_squareSqrt x = sqrt x ^ 2 == x

Now we can run QuickCheck over and over again and eventually we can
start to trust that `sqrt` is correct. In a DT language, we prove that
this irrefutably holds in every case!

    correctSqrt : Π (x : Double) (sqrt x ^ 2 ≡ x)

Now we absolutely know for certain that this property holds
*everywhere* and that `sqrt` is correct. Such is the magic of formal
verification.

This may seem impractical, but in actuality it does scale well. People
have proven entire industry strength C compilers to be correct!

Last summer I proved a compiler for something like STLC correct. I
actually bothered to run this compiler exactly twice, both times for
demonstrations. Why would I bother to run it more? I'd proven it to be
correct :)

## Wrap Up

Hopefully you've now got a slightly clearer idea about what these
dependent type things are and why one would want to use them. If
you've found this interesting you're probably wondering where to go to
learn more about these things.

I'd strongly recommend learning either Idris or Coq. Coq has the most
written material and documentation available. Idris comes closest to
being a useful language with dependent types. For Idris look no
further than the [main website](http://www.idris-lang.com).
For Coq, I'd urge you to take a look at
[Certified Programming with Dependent Types][CPDT]. Perhaps at some
point I'll write a more detailed post on resources for learning about
dependent types.

Best of luck and happy hacking.

[halting-oracles]: /posts/2014-07-30-many-shades-of-halting.html
[CPDT]:            http://adam.chlipala.net/cpdt/
