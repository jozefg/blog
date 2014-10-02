----
title: Learn You Some Category Theory
tags: math
----

### Introduction

Hi!

This is the first in a series of posts on basic category theory in Haskell.
they require no knowledge of category theory, but I expect the reader
to be familiar with Haskell. I will make various parallels to set theory,
but they can be skipped in favor of the corresponding Haskell code.

### Categories

What is a category?

A category is a collection of 2 things: objects and arrows.
We leave the idea of an object abstract, it's just a "thing". It varies from
category to category, in some it will be sets, in some monoids, whatever, but
it's the core "building block" of that category.

Arrows are different "things" that go from objects to object, we'd write them like
`f : A -> B` to mean that `f` is an arrow from object `A` to object `B`.

Think of this like a directed graph, objects are nodes and arrows are lines.
In fact as we go, we'll talk about parts of categories just like this, as diagrams.

Let's do some examples, let's say objects are sets. So

    A = {1, 2}
    B = {3, 4}

Then arrows would be functions from set to set.

    f 1 = 2
    f 2 = 4

Another example: Haskell. Objects would be types and arrows would be functions

``` haskell
    type A = Int
    type B = Int

    f :: A -> B
    f x = x + 1
```

Now for some group of objects and arrows to be a category, a few conditions have to hold

  - There must be an identity arrow for each object

        idA : A -> A
        idB : B -> B

  - There must be an operation to compose arrows, I use `.` for this

        f : A -> B
        g : B -> C
        g . f : A -> C

  - Identities and composition have to play nice,

        f . id = f
        id . f = f

And viola! If we can show these few things, it's a category!

#### Examples

First let's do the category of sets, which we call `Set`,

  1. Objects are sets
  2. Arrows are functions from set to set
  3. Identity arrows are just identity functions
  4. Arrow composition is function composition
  5. It's trivial To see that our identity arrows satisfy these conditions

Next is Haskell, this category is called `Hask`.

  1. Objects are types
  2. Arrows are functions
  3. The identity arrows are all given by `id`
  4. Arrow composition is just `.`
  5. We know that `id . f = f . id = f` in Haskell

There you have it, our start into the wide world of category theory.
