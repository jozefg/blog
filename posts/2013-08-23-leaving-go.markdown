-----------------
title: Leaving Go
tags: Go
-----------------
I've been using Go since Novemember and I've decided that it's time to give it
up for my hobby projects. I'd still be happy to use it professionally, but
I find that programming in Go isn't "fun" in the same way that Python, Haskell,
or Lisp is.

## Go, The Good
The best part about Go isn't actually Go. The community and infrastructure
around it are excellent. The command line `go` tool really is nice.

By far my favorite part is `go get`. Package management is something that
many a community has failed to address but Go seems to have handled it nicely.

This isn't shocking I suppose. Go was definitely made by engineers to solve
a very real world problem. I haven't used Go for a project with 10 or 20 people
but I suspect it would scale marvelously.

On the squishier side, Go's community is reasonably friendly. No newbie's got
their heads bitten off as far as I could see.

## Go, The Not So Good
While the community for Go is great, the language is ehhh. Unfortunately,
when I'm working on hobby projects, this is 80% of my concern. VB has good
support, but I'm not hacking it.

The two main issues I have with Go are

  1. The Type System
  2. Extensibility

### The Type System
Go's type system is well... lacking as it stands right now. The main
problem is that Go provides no safe system for polymorphism.

I'll give you a trivial example, define a generic absolute value function
in Go.

``` Go
     func abs(x ???) ???{
         ???
    }
```

Now what are those `???` supposed to be? Well, we have no notion
of parametric polymorphism so our only choice is subtyping polymorphism.

``` Go
     func abs(x interface{}) interface{} {
         ???
    }
```

So now that we've just taken all our lovely, optimization friendly type
information and thrown it away, let's manually get it back!

``` Go
    type Top interface{}
    func abs(x Top) Top {
        switch x.(type){
	        case int32:
    		    if x.(int32) < 0 {
			        return -x.(int32)
		        } else {
    			    return x.(int32)
		        }
	        case int64:
        		if x.(int64) < 0 {
			        return -x.(int64)
		        } else {
        			return x.(int64)
		        }
	        case float32:
        		if x.(float32) < 0 {
			        return -x.(float32)
		        } else {
        			return x.(float32)
		        }
	        case float64:
        		if x.(float64) < 0 {
			        return -x.(float32)
		        } else {
                            return x.(float64)
		        }
            }
        return nil
    }
```

Holy boilerplate batman! And using this means we are forced to stick
a cast right in the middle of our perfectly safe code.

By the way, there's an error in the above code? Did you catch it? It's
tricky because with all this code duplication you tend to just skim over
the boilerplate and miss the nasty runtime errors.

A type system that regularly requires casts is just gross, it's a sign
that the type system isn't expressive enough to describe a problem.

What would happen if we wrote this in Haskell?

```Haskell
    abs :: Num a => a -> a
    abs a = if a < 0 then -a else a
```

See the difference? And the Haskell version is extensible and cast free,
it'll work for any user defined types.

Now let's be fair to Go, we can try this

```Go
     type Abser interface{
         func Negate() Abser
         func LtZero() Abser
     }
     func Abs2(x Abser) Abser{
         if x.LtZero() {
             return x.Negate()
         }
         return x
     }
```
But this still isn't close to Haskell's version for several reasons, the
biggest one for me is that this version takes in some `Abser` and returns
some `Abser`. Are those the same underlying implementations? Who knows!

So we still have an unsafe cast in there just to use it because we
have no way of statically verifying that we're getting the same underlying
type back.

This kills any chance of safely composing functions that take in different
interfaces, for example, if we had a function over `int32`s, we couldn't
do `someFunc(abs(x))` because we'd have to stick our cast in there,
`someFunc(abs(x).(int32))`. Now we're just asking for trouble when
there's some error in the function that leads to a casting failure.

Doing this safely in Go looks like this,

    newX, err := abs(x).int(32)
    if err != nil {
        fmt.Println("Darn it!")
        // Handle errors
    }
    someFunc(newX)

Now if that doesn't grind on you I really don't know what would.

I don't mind dynamic typing and the possibility of runtime errors,
Python is fun to program in just like Haskell. But Go is imposing
all the pain of static typing with pretty much none of the benefits.

The response of the Go community is "Abs is a 2 line function, just do it
inline or per type" to which I respond: I want to define a generic algorithm,
or datastructure, or really anything reasonably complex!

When I started Go, I thought this was just me missing a few clever tricks for
how to properly utilize Go, I'm not so sure anymore.
The entire Go math library requires casts to `float64`s to use, using a stack in Go
requires casts from `interface{}`.

Coming from Haskell and Coq, this is not something I should have to put up with
in 2013.

### Extensibility
Consider the keyword `range`. It's a deeply magical keyword that only works
inside `for` loops on Go's primitive datastructures.

I like writing compilers so I end up dealing a lot with trees, want to have
`range` traverse your AST for you? Tough!

This is just one example of many

  - Only Go's primitive types my parameterize over other types
  - Only magical primitive functions may return 1 or 2 arguments depending on context
  - Only magical primitives have real parametric polymorphism
  - Only Go's primitives may have infix operators
  - and on and on and on!

These are all hitting the same problem, Go is not extensible. There simply isn't
a way to define a type and expect it to be as pleasant to use as a slice.

This apparently doesn't bother Go's maintainers, presumably because they
designed Go and deal with problems which slices, maps, and chans model
beautifully. For the rest of the world, it's a pain in the butt.

Guy Steele gave a wonderful talk about "growing a language". The core
idea was to start with a small but very extensible language and allow
*users* to determine which features are added.

The idea is that there's simply no way that any group of designers could
imagine how people will want to use their language so making it easy to
extend solves the problem wonderfully.

In Lisp, CLOS (Common Lisp Object System) was originally a library. It was
a user defined abstraction that was so popular it was ported into the standard.

Go is just the opposite. Any user defined abstractions are painfully,
intruisively obvious. Go developers seems to consider this a "Good Thing".
On one hand it does aid code readability. On the other, it really limits
what Go's pleasant to use for.

As a trivial case study. Imagine we wanted to use Go for some form of scientific computing.
We'd need some sort of Bignum type because `int64` ain't gonna hack it. In Python or Haskell,
here's how you add 2 bignums,

    a + b

Here's how you do it in Go,

    a.Add(b)

Ok, it's only a few characters, big deal. Now what does this do?

    b.Mul(b).Sub(big.NewInt(4).Mul(a).Mul(c))

Or in Haskell

    b*b - 4 * a * c

Which would you rather write?

I can't help but feel like Go was designed with only problems the designers were facing in mind.
This is great for them, but calling Go a general purpose language should
mean that it's nice to use for other sorts of problems too.

## Conclusion

I'm really sad to have written this actually. I wanted to like Go a lot. I wanted a fast, compiled
replacement for stuff I write in C right now. But Go is not that language. Shame.

Thank you to the Go team for all the hardwork on the project and best of luck.
