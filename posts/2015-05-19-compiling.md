---
title: Compiling a Lazy Language in 1,000 words
tags: compilers, haskell
---

I'm a fan of articles like [this one][git] which set out to explain a really
complicated subject in 600 words or less. I wanted to write one with a similar
goal for compiling a language like Haskell. To help with this I've broken down
what most compilers for a lazy language do into 5 different phases and spent 200
words explaining how they work. This isn't really intended to be a tutorial on
how to implement a compiler, I just want to make it less magical.

I assume that you know how a lazy functional language looks (this isn't a
tutorial on Haskell) and a little about how your machine works since I make a
few references to how some lower level details are compiled. These will make
more sense if you know such things, but they're not necessary.

And the word-count-clock starts... now.

## Parsing

Our interactions with compilers usually involve treating them as a huge function
from string to string. We give them a string (our program) and it gives us back
a string (the compiled code). However, on the inside the compiler does all sorts
of stuff to that string we gave it and most of those operations are inconvenient
to do as string operations. In the first part of the compiler, we convert the
string into an abstract syntax tree. This is a data structure in the compiler
which represents the string, but in

 1. A more abstract way, it doesn't have details such as whitespace or comments
 2. A more convenient way, it let's the compiler perform the operations it wants
    efficiently

The process of going String -> AST is called "parsing". It has a lot of (kinda
stuffy IMO) theory behind it. This is the only part of the compiler where the
syntax actually matters and is usually the smallest part of the compiler.

Examples:

 - [Purescript][purs-parse]
 - [Elm][elm-parse]

## Type Checking

Now that we've constructed an abstract syntax tree we want to make sure that the
program "makes sense". Here "make sense" just means that the program's types are
correct. The process for checking that a program type checks involves following
a bunch of rules of the form "A has type T if B has type T1 and C has
type...". All of these rules together constitute the type system for our
language. As an example, in Haskell `f a` has the type `T2` if `f` has the type
`T1 -> T2` and `a` has the type `T1`.

There's a small wrinkle in this story though: most languages require some type
inference. This makes things 10x harder because we have to figure the types of
everything as we go! Type inference isn't even possible in a lot of languages
and some clever contortions are often needed to be inferrable.

However, once we've done all of this the program is correct enough to
compile. Past type checking, if the compiler raises an error it's a compiler
bug.

Examples:

 - [A type inferencer for Mini-ML][mini-ml]

## Optimizations/Simplifications

Now that we're free of the constraints of having to report errors to the user
things really get fun in the compiler. Now we start simplifying the language by
converting a language feature into a mess of other, simpler language
features. Sometimes we convert several features into specific instances of one
more general feature. For example, we might convert our big fancy pattern
language into a simpler one by elaborating each `case` into a bunch of nested
`case`s.

Each time we remove a feature we end up with a slightly different language. This
progression of languages in the compiler are called the "intermediate languages"
(ILs). Each of these ILs have their own AST as well! In a good compiler we'll
have a lot of ILs as it makes the compiler much more maintainable.

An important part of choosing an IL is making it amenable to various
optimizations. When the compiler is working with each IL it applies a set of
optimizations to the program. For example

 1. Constant folding, converting `1 + 1` to `2` during compile time
 2. Inlining, copy-pasting the body of smaller functions where they're called
 3. Fusion, turning multiple passes over a datastructure into a single one

Examples:

 - [Pattern matching][pat-match]
 - [A nice demonstration of many ILs][system-f]

## Spineless, Tagless, and Generally Wimpy IL

At some point in the compiler, we have to deal with the fact we're compiling a
lazy language. One nice way is to use a spineless, tagless, graph machine (STG
machine).

How an STG machine works is a little complicated but here's the gist

 - An expression becomes a closure/thunk, a bundling of code to compute the
   expressoin and the data it needs. These closure may depend on several
   arguments being supplied
 - We have a stack for arguments and another for continuations. A continuation
   is some code which takes the value returned from an expression and does
   something with it, like pattern match on it
 - To evaluate an expression we push the arguments it needs onto the stack and
   "enter" the corresponding closure, running the code in it
 - When the expression has evaluated itself it will pop the next continuation
   off the stack and give it the resulting value

During this portion of the compiler, we'd transform out last IL into a C-like
language which actually works in terms of pushing, popping, and entering
closures.

The key idea here that makes laziness work is that a closure defers work! It's
not a value, it's a recipe for how to compute a value when we need it. Also
note, all calls are tail calls since function calls are just a special case of
entering a closure.

Another really beautiful idea in the STG machine is that closures evaluate
themselves. This means closures present a uniform interface no matter what, all
the details are hidden in that bundled up code. (I'm totally out of words to say
this, but screw it it's really cool).

Examples:

 - [My writeup][stg]
 - [ezyang's (better) writeup][ezyang]
 - [The paper][stock]
 - [Another paper][curry]

## Code Generation

Finally, after converting to compiling STG machine we're ready to output the
target code. This bit is very dependent on what exactly we're targeting.

If we're targeting assembly, we have a few things to do. First, we have to
switch from using variables to registers. This process is called register
allocation and we basically slot each variable into an available register. If we
run out, we store variables in memory and load it in as we need it.

In addition to register allocation, we have to compile those C-like language
constructs to assembly. This means converting procedures into a label and some
instructions, pattern matches into something like a jump table and so on. This
is also where we'd apply low-level, bit-twiddling optimizations.

Examples:

 - [LLVM Code Generation][sdiehl]
 - [Any good compilers book][ml]

## Conclusion

Okay, clock off.

Hopefully that was helpful even if you don't care that much about lazy languages
(most of these ideas apply in any compiler). In particular, I hope that you now
believe me when I say that lazy languages aren't magical. In fact, the worry of
how to implement laziness only really came up in one section of the compiler!

Now I have a question for you dear reader, what should I elaborate on? With
summer ahead, I'll have some free time soon. Is there anything else that you
would like to see written about? (Just not parsing please)

[git]: http://maryrosecook.com/blog/post/git-in-six-hundred-words
[purs-parse]: https://github.com/purescript/purescript/blob/master/src/Language/PureScript/Parser/
[elm-parse]: https://github.com/elm-lang/elm-compiler/tree/master/src/Parse
[mini-ml]: /posts/2015-02-28-type-inference.html
[pat-match]: http://www.cs.tufts.edu/~nr/cs257/archive/luc-maranget/jun08.pdf
[system-f]: http://www.cs.cornell.edu/talc/papers/tal-popl.pdf
[stg]: /posts/2014-10-28-stg.html
[ezyang]: http://blog.ezyang.com/2011/04/the-haskell-heap/
[stock]: http://research.microsoft.com/apps/pubs/default.aspx?id=67083
[sdiehl]: http://www.stephendiehl.com/llvm/
[ml]: http://www.cs.princeton.edu/~appel/modern/ml/
[curry]: http://research.microsoft.com/en-us/um/people/simonpj/papers/eval-apply/index.htm
