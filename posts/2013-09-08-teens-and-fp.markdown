----
title: Teens and Functional Programming
----

As a teenager who spends most of his time programming, I spend
a lot of time interacting/teaching other teens interested in
programming.

One thing that's always bothered me is the lack of fellow
teenage functional programmers. I've been wondering whether
this is simply chance? Or is there something that makes
functional programming bad for beginners? Especially teenage
ones.

Well first let's formally define what I mean by a functional
language, the definition is always a bit fuzzy. When I refer
to a functional language, I mean a language that:

   - Has primarily immutable data (Convention or enforced)
   - Functions as first class values
   - Functions are primarily "pure functions"

Notice that I left out

   - Type Systems
   - Algebraic data types + Pattern matching
   - Purity

### Immutability

Perhaps immutability is the problem. It's certainly weird to experienced
imperative programmers. But what about for beginners?

I'd argue immutability is actually pretty natural for a new programmer. It means
variables are just names for data. No more of that confusing "a variable is like
a box that you can put values in" explanation. Especially once you get into
subtleties like indirection mutation looks less appealing.

In the first course for computer science students at the University of Minnesota,
there is a whole quiz filled with problems like this in Python:

    # What is the output of
    a = [1, 2, 3]
    b = a[1:]
    b[1] = 4
    print a

Once you have a whole quiz devoted to a topic, it's safe to say that it's
confusing.

This class also taught Scheme, when it came time to explain `let`, the professor
simply said

> This is `let`.
>
>     (let ((a 1))
>         (+ 1 a))
>
> Just substitute `a` for 1 within the parens.

and that was that.

I'd posit that the reason is that with immutability you can simply substitute
a name for its value and have unchanged semantics. With mutable variables,
a variable is more than just it's value.

Now that's not to say immutability doesn't get weird eventually, purely functional
data structures do sometimes require some mental gymnastics, but there is certainly
not more mental overhead than with imperative programming's pervasive mutation.

### First Class Functions

I find it hard to believe that first class functions are the problem since
they're not that unique anymore. Python, Ruby, and JavaScript all have them
and they're hugely popular with beginners.

The other thing is that you can largely ignore them until you need them. No one
will make you use map, you could write the stupid repetitive recursion out every time.
Additionally, many languages provide some sort of construct to let you avoid
`map`, `filter`, or whatever. For example, in Haskell

    filter even . map (+1) $ [1..10]
    [x + 1 | x <- [1..10], even x]

Perfect for a beginner. In fact, Python stole these for precisely this reason.

Finally, they actually alleviate a lot of complexity. Look at anonymous classes and
the strategy pattern. The whole thing is a very large, ugly hack for dealing with the
lack of first class functions.

### Pure Functions

This one isn't too hard to argue. If you have some function `f`, if you call it

    a = f(1);
    b = f(1);

You'd really expect it to give you back the same thing twice. This has what people have
come to expect from math. It's much the same argument that I made for immutability, with
a pure function there's all sorts of nice assumptions you can make about it, once again
a function application is just becomes a name for the resulting value.
This is different than a function call in python where the actual computation
is important because it has side effects.

I said "primarily pure" because some things are really impure, the classic example being `readLine`.

    print "Enter your age:";
    age = readLine();
    print "Enter your height:";
    height = readLine();

Now if we'd hope that `age` and `height` contain different values. In Haskell we have monads for
this, but those are notoriously hard to understand. Instead, pragmatic impurity is probably the
best course for a beginner's language. Much like Scheme.

## But Objects!

Now I know that someone is thinking, "But object orientation!!". To them I say, you're right:
for some set of problems object orientation is a better model for a problem. But it's a far smaller
set of problems than you'd think.

It's important for a beginner to be exposed to multiple paradigms, but I see no reason why the
first paradigm shouldn't be functional. In fact, I've outlined several reasons why it *should*
be functional.

-----------------------------------------------------------------------------------------------

# But Which One?

So if you're some random teen about to start functional programming, which language
would you choose?

Some of the more popular languages that fit my definition of functional:

- Scheme/Racket
- Clojure
- Haskell
- Erlang
- Scala
- SML
- OCaml
- F#

Notice that most of the common "pseudo-functional" languages (JavaScript, Ruby, etc) fail the
first constraint of immutability. I also chose to leave off some of the more research oriented
languages (Coq, Agda, etc) because we're talking about beginners here.

Now for a language to be good for a beginner it has to

- Not have an overly complicated type system
- Have an implementation with good error messages
- Have lots of libraries, particularly web/game frameworks
- A batteries included standard library to get up and running with
- Have good community support **for newcomers**

### Haskell
We certainly can agree that Haskell is not a beginner's language.
Now Haskell is the language I write 99% of my code in and I'm saying this.

The type system, laziness, and monadic IO concept are all very daunting to
a beginner. It doesn't help that some of the error messages GHC produces
are well... opaque. It's not a bad language, but it's not one that I would
suggest for starting with.

### SML/OCaml
Now SML and OCaml are both fine languages. But they don't have the infrastructure
to support a lot of teenage programmers.

They're missing the game/GUI/web frameworks. They're missing the tools. They're
missing the libraries. It's just not there.

Other than that though, I see no reason why OCaml in particular wouldn't make
quite a reasonable language to start with. Pragmatically functional, reasonable
type system, and strict semantics.

But the core language isn't enough to make it a good first choice.

### Scala

Scala, like Haskell, is a very nice language that
definitely fails the simplicity criteria. The type
system is just as sophisticated as Haskell's but with
worse inference you have to be more explicit.

Again, it's just not a language for a beginner from
what I've seen.

Though with access to the java ecosystem and a very
active community it nicely passes every other criteria.

### Erlang

Erlang may be a nice language for a beginner. Once again
I'll admit ignorance and leave it to someone else to comment
on it's suitability.

I suspect that the focus on concurrency will make it a bit
less intuitive to a beginner who doesn't have any interest in those issues.

### Racket/Scheme

Scheme on its own just fails the library support. It's good for a classroom
but for the demanding teenage hacker, the lack of game/gui frameworks is just
killer.

Racket is a different story. Racket actually has a good set of libraries for
GUI's and games. It's simple enough for a beginner. It has a good community
for beginners, being made by educators.

In fact, the only problem I see with Racket vs Python is just the lack of hype.
There isn't the same marketing going on for Racket as Python, but there certainly
could be.

Racket is what I recommend to people who are interested in starting with functional
programming.

### Clojure

I think Clojure is another strong choice. It's just as simple as Racket and a much
better community behind it.

It's got all of Java's libraries for games and several web frameworks of it's own.
In particular I'd love to see someone write a really slick DSL for minecraft in Clojure.
It would be a great hook to say "Hey, come learn Clojure because it makes writing
minecraft mods trivial".

Maybe in the future I'll start recommending Clojure instead of Racket. It looks
like it's got a brighter future with the much stronger community drive behind it.

-----------------------------------------------------------------------------------

# Conclusion

So where does this leave us? Well I think the answer to the original question is clear.
Functional programming isn't the problem, functional languages are the problem.

There isn't a clear analog to something like Python or Ruby in the
functional programming world. I think it's a legitimate niche for a language to try
to fill too.

Perhaps I've overlooked something, but right now I think functional programming has got
a ways to go making it more accessible to beginners. And I think it's definitely worth the
effort.

