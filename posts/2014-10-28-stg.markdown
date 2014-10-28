---
title: The Guts of a Spineless [Tagless] G-Machine
tags: haskell
---

It's fairly well known that Haskell is a bit um.. different then how
stock hardware sees the world. As such, compiling Haskell (and indeed
most functional languages) presents an interesting challenge. How do
we go about mapping our lovely highlevel constructs to instructions
without sacrificing efficiency? To this end something called the
"spineless tagless G-machine" is employed by GHC and a few
other functional languages to compile to stock hardware. In this post
I'll go over some of the basic ideas behind STG, hopefully in the
process you'll learn a bit about how Haskell is implemented.

### Core Concepts

The basic idea behind a compiler intent on going the STG route is
something like

 1. .. front end stuff ..
 2. Translate IL to STG language
 3. Compile STG language to C/ASM/LLVM/Javascript

In GHC case I understand the pipeline is something like

 1. Parsing
 2. Typechecking
 3. Desugaring + a few bobs and bits
 4. Translation to core
 5. Lion share of optimization
 6. Translation to STG language
 7. STG language to C--
 8. C-- to assembly

We're really concerned with parts 6 and 7 here. First things first,
let's lay out what's exactly in the STG language. It's a tiny
functional language that looks a bit like Haskell or Core, with a few
restrictions. A program is simply a series of bindings, much like
Haskell. The top levels look something like

    f = {x y z} flag {a b c} -> ...

You should read this for now as `f = \a b c -> ...`. The first set of
variables and the flag correspond to some stuff we'll discuss later.

Inside the `...` we can write most of what you would expect form
Haskell. We have let[rec] bindings, case expressions, application,
constructors, literals, and primitives. There is a caveat though,
first off all constructor applications must be fully saturated. This
isn't unlike OCaml or something where you can't just treat a
constructor as a function with an arbitrary name. We would write

    \a -> Just a

instead of just `Just`. Another bit of trickiness, our language has no
lambdas! So we can't even write the above. Instead if we had something
like

     map Just [1, 2, 3]

We'd have to write

     let f   = \a -> Just a
         l'' = 3 : nil
         l'  = 2 : l''
         l   = 1 : l'
     in map f l

The reason for the awkward `l''` series is that we're only allowed to
apply constructors and functions to atoms (literals and variables).

One other noteworthy feature of STG is that we have primitive
operations. They need to be fully saturated, just like constructors,
but they work across unboxed things. For example there would probably
be something like `+#` which adds to unboxed integers. To work with
these we also have unboxed literals, `1#`, `2#`, so on and so on.

No despite all these limitations placed on STG, it's still a pretty
stinking highlevel language. There's letrec, higher order functions, a
lot of the normal stuff we'd expect in a functional language. This
means it's not actually to hard to compile something like Haskell or
Core to STG (I didn't say "compile well").

As an example, let's look at translating factorial into STG
language. We start with

    f :: Int -> Int
    f i = case i of
      0 -> 1
      i -> i * (f (i - 1))

Now the first step is we change the binding form

    f = {} n {i} -> ...

The case expressions clause can remain the same, we're already casing
on an atom

    case i of
      (MkInt# i#) -> ...

Now comes the first big change, our boxed integers are going to get in
the way here, so the case expression strips away the constructor
leaving us with an unboxed integer.

     case i of
       MkInt i# -> case i# -# 1# of
           dec# ->
             let dec = \{dec#} u {} -> MkInt dec#
             in case fact dec of
             MkInt rest# -> case i# * rest# of
               result# -> MkInt result#


Now we can see what those extra {}'s were for. They notate the free
variables for a thunk. `dec` for example has a free variable `dec#`
and it exists to box that result for the recursive call to
factorial. We use `case` expressions to get evaluation. Most programs
thus become chains of `case`'s and `let` alternating between creating
thunks and actually doing work.

That `u` in between the {}'s in `dec` was also important. It's the
update flag. Remember how in Haskell we don't want to force the same
thunk twice. If I say

    let i = 1 + 1 in i + i

We should only evaluate `1 + 1` once. That means that the thunk `i`
will become has to do some updating. The update flag signifies the
difference between thunks that we want to update and thunks that we
don't. For example, if we replaced the thunk for `+` with the first
result it returned, we'd be mighty surprised. Suddenly `1 + 1 + 1` is
just 2!

The `u` flag says "yes, I'm just a normal expression that should be
updated" and the n flag says the opposite.

That about wraps up our discussion of the STG language, let's talk
about how to implement it now.

### Semantics

This language wouldn't be much good if it didn't lend itself to an
easy implementation, indeed we find that the restrictions we placed
upon the language prove to be invaluable for its compilation (almost
like they were designed that way!).

In order to decide how best to implement it, we first let at the
formal semantics for our language. We give these semantics a tuple of
6 things.

 1. The code - the instruction we're currently executing
 2. The argument stack - A stack of integers or pointers to closures
 3. The return stack - A stack of continuations
 4. The update stack - A stack of update frames and sadness
 5. The heap - A map from addresses to closures
 6. The environment - A map from names to addresses of toplevel
    closures

A code is more or less the current thign we're attempting to do. It's
either

 1. `Eval e p` - evaluate an expression in an environment
 2. `Enter a` - Enter a closure
 3. `ReturnCon c ws` - Return a constructor applied to some arguments
 4. `ReturnInt` - Return an integer

Now the idea is we're going to "unroll" our computations into pushing
things onto the continuation stack and entering closures. We start
with the code `Eval main {}`. That is to say, we start by running
`main`. Then if we're looking at a `case` we do something really
clever

     EVAL(case expr of {pat1 -> expr1; ...}, p) as rs us h o

becomes

    EVAL (expr, p) as ({pat1 -> expr1; ...} : rs) us h o

That is to say, we just push the pattern matching on to the
continuation stack and evaluate the expression.

Presumably at some point we'll get to a "leaf" in our expression, some
random literal or constructor. At this point we make use of our
continuation stack

    EVAL (C ws, p) as ((...; c vs -> expr; ...) : rs) us h o
    ReturnCon (C ws) as ((...; c vs -> expr; ...) : rs) us h o
    EVAL (expr, p[vs -> ws]) as rs us h o

So our pattern matching is rolled into `ReturnCon`. `ReturnCon` will
just look on top of the return stack looking for a continuation which
wants its constructor and evaluate its expression, mapping the
constructor's variables to the pattern's variables.

The story is similar for literals

    EVAL (Int i, p) as ((...; c vs -> expr; ...) : rs) us h o
    ReturnInt i as ((...; i -> expr; ...) : rs) us h o
    EVAL (expr, p) as rs us h o

Another phase is how we handle let's and letrec's. In this phase
instead of dealing with continuations, we allocate more thunks onto
the heap.

    EVAL ((let x = {fs} f {xs} -> e; ... in expr), p) as rs us h o
    EVAL e p' as us h' o

So as we'd expect, evaluating a let expression does indeed go and
evaluate the body of the let expression, but changes up the
environment in which we evaluate them. We have

    p' = p[x -> Addr a, ...]
    h' = h[a -> ({fs} f {xs} -> e) p fs, ...]

In words "the new environment contains a binding for `x` to some
address `a`. The heap is extended with an address `a` with a closure
`{fs} f {xs} -> ...` where the free variables come from `p`". The
definition for letrec is identical except the free variables come from
`p'` allowing for recursion.

So the STG machine allocates things in lets, adds continuations with
case, and jumps to continuation on values.

Now we also have to figure out applications.

    EVAL (f xs, p) as rs us h o
    ENTER a (values of xs ++ as) rs us h o

where the value of `f` is `Addr a`. So we push all the arguments
(remember they're atoms and therefore trivial to evaluate) on to the
argument stack and enter the closure of the function.

How do we actually enter a closure? Well we know that our closures are
of the form

    ({fs} f {vs} -> expr) frees

If we have enough arguments to run the closure (length vs > length of
argument stack), then we can just `EVAL expr
[vs -> take (length vs) as, fs -> frees]`. This might not be the case
in something like Haskell though, we have partial application. So what
do we do in this case?

What we want is to somehow get something that's our closure but also
knows about however many arguments we actually supplied it. Something
like

    ({fs ++ supplied} f {notSupplied} -> expr) frees ++ as

where `supplied ++ notSupplied = vs`. This updating of a closure is
half of what our update stack `us` is for. The other case is when we
*do* actually enter the closure, but `f = u` so we're going to want to
update it. If this is the case we add an update from to the stack
`(as, rs, a)` where `as` is the argument stack, `rs` is the return
stack, and `a` is the closure which should be updated. Once we've
pushed this frame, we promptly empty the argument stack and return
stack.

We then add the following rules to the definition of `ReturnCon`

    ReturnCon c ws {} {} (as, rs, a) : us h o
    ReturnCon c ws as rs us h' o

where `h'` is the new heap that's replaced our old closure at `a` with
our new, spiffy, updated closure

    h' = h[a -> ({vs} n {} -> c vs) ws]

So that's what happens when we go to update an updateable closure. But
what about partial application?

    Enter a as {} (asU, rs, aU) : us h o
    Enter a (as ++ asU) rs us h' o

where

    h a = ({vs} n {xs} -> expr) frees
    h' = h [aU -> ((vs ++ bound) n xs -> e) (frees ++ as)]

This is a simplified rule from what's actually used, but gives some
intuition to what's happening: we're minting a new closure in which we
use the arguments we've just bound and that's what the result of our
update is.

### Compiling This

Now that we have some idea of how this is going to work, what does
this actually become on the machine?

The original paper by SPJ suggests an "interpreter" approach to
compilation. In other words, we actually almost directly map the
semantics to C and call it compiled. There's a catch though, we'd like
to represent the body of closures as C functions since they're
well.. functions. However, since all we do is enter closures and jump
around to things till the cows come home, it had damn well better be
fast. C function calls aren't built to be that fast. Instead the paper
advocates a tiny trampolining-esque approach.

When something wants to enter a closure, it merely returns it and our
main loop becomes

     while(1){cont = (*cont)();}

Which won't stackoverflow. In reality, more underhanded tricks are
applied to make the performance suck less, but for we'll ignore such
things.

In our compiled results there will be 2 stacks, not the 3 found in our
abstract machine. In the first stack (A-stack) there are pointer
things and the B-stack has non-pointers. This are monitored by two
variables/registers `SpA` and `SpB`which keep track of the heights of
the two stacks. Then compilation becomes reasonably straightforward.

An application pushes the arguments onto the appropriate stacks,
adjusts Sp*, and enters the function A let block allocates each of the
bound variables, then the body. Entering a closure simply jumps to the
closures code pointer. This is actually quite nifty. We delegate all
the work of figuring out exactly what `Enter` will do (updates,
continuation jiggering) is left to the closure itself.


A case expression is a bit more complicated since a continuation's
representation involves boxing up the local environment for each
branch. Once that's bundled away, we represent a continuation as a
simple code pointer. It is in charge of scrutinizing the argument
stack and selecting an alternative and then running the appropriate
code. This is a lot of work, and unless I'm crazy will need to types
of bound variables for each branch (really just ptr/non-ptr). The
selection of an alternative would be represented as a C switch,
letting all sorts of trickery with jump tables be done by the C
compiler.

In order to return a value, we do something clever. We take a
constructor and point a global variable at its constructor closure,
containing its values and jump to the continuation. The continuation
can then peek and poke at this global variable to bind things as
needed for the alternatives. There is potentially a massive speedup by
returning through registers, but this is dangerously close to work.

From here, primitive operations can be compiled to
statements/instructions in whatever environment we're targeting. In C
for example we'd just use the normal `+` to add our unboxed integers.

The last beast to slay is updates. We represent update frames by
pointers to argument stacks and a pointer to a closure. That means
that the act of updating is merely saving `Sp*` in an update from,
clobbering them, and then jumping into the appropriate closure. We
push the update from onto stack B and keep on going.

I realize that this is a glancing overview and I'm eliding a lot of
the tricky details, but hopefully this is sufficient to understand a
bit about what going on at an intuitive level.

### Wrap Up

So now that you've put all the effort to get through this post, I get
to tell you it's all lies! In reality GHC has applied all manner of
tricks and hacks to get fast performance out of an STG model. To be
honest I'm not sure where I should point to that explains these tricks
because well... I have no idea what they are.

I can point to

 - [SPJ's original paper][paper]
 - [The Relevant GHC Wiki Page][wiki]

If you have any suggestions for other links I'd love to add them!

[wiki]: https://ghc.haskell.org/trac/ghc/wiki/Commentary/Compiler/GeneratedCode
[paper]: http://research.microsoft.com/~simonpj/papers/spineless-tagless-gmachine.ps.gz
