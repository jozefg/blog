# Code & Co

This is the source for my [blog][blog]. I write about type theory,
functional programming, and other things I waste my life on.

Contributions welcome.

### Usage

This site is cabalized so getting it set up is simple if you've got a
Haskell environment.

    blog/ $ cabal sandbox init
    blog/ $ cabal configure; cabal install --only-dependencies
            ... Drink coffee while Hakyll downloads ...
    blog/ $ cabal run watch

And then there will be a nice little local site running on
`localhost:8000` and Hakyll while update it automatically upon any
change to a file.

[blog]: http://jozefg.bitbucket.org
