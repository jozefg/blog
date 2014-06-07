---
title: Overview of A Scheme Compiler
---

For the last few months I've been spending a fair amount of time on a
fun little Scheme to C compiler,
[`c_of_scheme`](http://bitbucket.org/jozefg/c_of_scheme).

In this post I'll outline the high level overview of `c_of_scheme` and
in future posts detail the specifics of each component.

## Modules
`c_of_scheme` is divided into 11 modules: 2 utility modules, 6
modules which each handle one step of compilation, a module with definitions
of ASTs, a driver, and of course Main.

First, let's discuss the utility modules, `Utils.Gen` and `Utils.Error`. `Gen`
defines a monad, `Gen`. This is used to generate unique integers to
be used as identifiers. For example:

``` haskell
    data Var = Name String | Gen Integer

    genVar :: Gen Var
    genVar = Gen <$> gen
```

Other stages of the compiler (continuation passing style, closure
conversion, and lambda lifting) need lots of temporaries so this is
used throughout the compiler.

`Gen` also comes with a monad transformer that implements a handful of
useful MTL type classes. Overall, nothing too stunning.

The other uninteresting utility module is `Error`, this is just a
wrapper around `Either` with a few functions for throwing errors and
good pretty printing of errors. This is used internally to signal a
major internal error.<!-- could just end this here? -->

The precise interface is given by a set of functions `failRW`,
`failCPS`, `failClos`, etc., which correspond to each stage of
compilation. These generate lovely pretty printed error messages for
each stage. This will become clearer as we go over each phase
individually and it's clear what needs to signal failure.

A module that's worth mentioning that's not a compilation stage but
not quite a utility module is `AST`. This defines the various abstract syntax trees and
primops for our representation of Scheme. This also defines the
compiler monad, which combines our error monad with `Gen` and some
other bits and bobs useful for our compiler. More on `AST` in future posts.

## Stages of Compilation

Now let's actually go over the individual phases of compilation.

### Parsing (Parser.hs)

This is the least interesting phase of compilation.. I personally just
dislike parsing so I don't have much to say about this.

A legal Scheme program is a list of definitions, we don't currently
allow top level expressions. We also don't currently support the usual
define sugar for functions.

The parser uses Parsec because I just happen to know the Parsec
API, ironically because of [this](http://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours).
If anyone cares enough to write a proper lexer and/or parser
or something, I'm more than happy to help!

### Rewrite Top Levels (RewriteTopLevel.hs)

This phase is a little peculiar. It exists because we're targeting C
and C has a fairly annoying restriction on what it allows top levels to
initialized to.

In C, we can't write something like

``` C
    int c = 1 + 1 + 1;
```

but in our dialect of Scheme, this is the only way to write
interesting computations! This phase of compilation rewrites top
levels (Shocking!) to match the C definition of top levels.

This is done by changing each definition to an `Init`, this will
later turn into a C declaration without initialization. Next we create
a new function, our main function, that is a series of assignments
which pair each top level definition to its initializer.

For example

``` scheme
    (define foo 1)
    (define bar 2)
    (define quux (+ foo bar))

    (define _ (display quux))
```

will become

``` scheme
    (init foo)
    (init bar)
    (init quux)
    (init _)

    (define magical-main
       (lambda ()
          (set! foo 1)
          (set! bar 1)
          (set! quux (+ foo bar))
          (set! _ (display quux))))
```

where `magical-main` will be the first thing called in the generated
code.

A caveat, we turn `(define foo (lambda (..) ...))` into something
different since it's more efficient to directly convert these to functions.

### Continuations Passing Style Conversion (CPS.hs)

This is the first interesting bit of compilation, CPS is a style where
each function call is a tail call. Here's an example non-CPS code
converted to CPS.

``` scheme
    (define foo
       (lambda (y)
          (+ 1 y)))

     (define foo-cps
       (lambda (cont x y)
         ((lambda (+')
            ((lambda (one)
               ((lambda (x')
                  ((lambda (result)
                     (cont result))
                   (+' one x')))
                x))
             1))
          +)))
```

Notice how with the CPS'ed version we've actually made evaluation
order explicit and have removed non-primitive expressions.

CPS.hs converts the AST to use CPS. We'll detail this process later
but for now I'll mention one more interesting tidbit.

CPS.hs is also where we implement call/cc! In fact it's trivial to do. All we do as add the declaration for

``` scheme
    (define call/cc
       (lambda (c f)
          (f c
             (lambda (ignored x) (c x)))))
```

### Optimizations (OptimizeCPS.hs)

This module implements the simple optimizations we perform. For now
this is limited to simple inlining and constant folding, but this
should improve in the future.

These optimizations are implemented quite pleasantly with recursion schemes.

### Closure Conversion + Lambda Lifting (ClosureConvert.hs)

This is the most difficult phase of compilation, for me anyways. In
concept it's quite simple though.

The idea is that we take the implicit closure "argument" that all
scheme procedures take and make it explicit. To this end we add three
new primops, `NewClos`, `ReadClos`, and `WriteClos`. These do much
what you would expect and let us treat closures opaquely as first
class values.

Next we change each procedure to take an extra argument, its closure,
and change closed over variables to be selected from this
closure. Finally we change each lambda to be paired with its closure
when constructed.

This sounded pretty feasible to me on paper, but in practice it seems
to be the greatest source of bugs in `c_of_scheme`. It finally seems
to work nicely now so I'll be sure to blog about it soon.

### Code Generation (CodeGen.hs)

This is the final stop in our compilation pipeline - we generate C
code.

To do this we use one of
[my libraries](http://hackage.haskell.org/package/c-dsl). This is
actually quite a simple step in the compiler since closure-converted,
CPS-ed code is quite close to C.

Some of the details that code generation handles:

 - Interfacing to the runtime system
 - Generating the main method
 - Generating declarations for all the variables used in our
   intermediate language
 - Mapping the Scheme variables to appropriate C names

While this might sound daunting, this isn't actually so bad.

### Driver (Driver.hs)

While I might not write a post on it, `Driver` is my personal favorite
module. It glues together all of the previous compilation phases and
provides a bunch of nice high level functions like `compileScheme`.

The reason I like it so much is that all the code in it is a very
nice, clean example of composing components as good old functions.

If you're looking to understand `c_of_scheme`'s particular
implementation, I'd urge you to start with `Driver`. It'll provide a
bit of an intuition from what goes to where.

## The Runtime System

Currently `c_of_scheme` has an incredibly naive runtime system. Mostly
because it's being written by an incredibly naive C programmer
(hi!).

I already [wrote](posts/2014-05-05-i-used-c-correctly.html)
about the most interesting bit of the RTS: tail calls.

I plan on talking a bit
about the RTS in the context of code generation (since it'd be
impossible not to), and perhaps a post on `c_of_scheme`'s simple little
mark and sweep GC.

## Wrap Up

So that's the high level overview of `c_of_scheme`, I think the
compiler is best exemplified by one particular function in `Driver.hs`:

``` haskell
    compileScheme :: [SDec UserPrim] -> Compiler [CExtDecl]
    compileScheme = addPrimops >=> makeMain >=> cpsify >=> optimizeCPS >=> closConvert >=> codegen
      where addPrimops = return . (++prims)
```

This chains together all the phases of compilation into one big old
function from the Scheme AST to the C one.

Now, if you're really interested in `c_of_scheme`, go ahead and grab
the source with

    hg clone ssh://hg@bitbucket.org/jozefg/c_of_scheme

I do use mercurial so you can also grab a zip from bitbucket if you're
unwilling to use mercurial for one command :)

I should have posts about each specific phase of compilation up in
Real Soon Now. I'll edit with a list of links to posts below as they
are written.
