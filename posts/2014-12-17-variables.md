---
title: Cooking λΠ 3 ways
tags: haskell, compilers
---

After my last post, I didn't quite feel like ending there. I was a
little dissatisfied with how binding was handled in the type checker,
the odd blend of HOAS, GUIDs, and DeBruijn variables was... unique.

In the post I explore 3 versions of the same code

 1. The original method
 2. Using `bound` to handle all binding
 3. Full HOAS

There's a lot of code in this post, enough that I think it's worth
hosting the code on its own. You can find it on [github][cooked-pi-github]
and [bitbucket][cooked-pi-bitbucket].

## The Original

I've already described most of the original method
[here][original-tc]. To recap

 1. Values were HOAS
 2. Terms were DeBruijn
 3. To bridge the gap, we had "free constants" randomly generated

The issue I had with this is we almost got the worst of all 3 worlds!
We were constantly bumping a counter to keep up with the free
constants we needed to generate. We had to muddy up the types of
values with *another* notion of free constants so we could actually
inspect variables under HOAS binders! And finally, we had to do the
painful and tedious substitutions on DeBruijn terms.

On the other hand, if you'd never used any of those binding schemes
together, you too can go triple or nothing and try to understand that
code :)

What I really wanted was to unify how I represented values and
terms. I still wanted a clearly correct notion of equality, but in
this way I could probably dodge at least two of the above.

The obvious thing to do would be to stick with DeBruijn variables and
just instantiate free variables with constants. This is ugly, but it's
moderately less horrible if we use a library to help us with the process.

## `bound`

So my first stab at this approach was with Edward Kmett's
[bound](http://hackage.haskell.org/package/bound). For those who
aren't familiar with this library, it centers around the data type
`Scope`. `Scope b f a` binds variables of type `b` in the structure
`f` with free variables of type `a`. The assumption is that `f` will
be a monad which represents our AST.

Further, `f` is parameterized over variables, it doesn't attempt to
distinguish between bound and free ones however. This means that `>>=`
corresponds to substitution. Then what `Scope` does is instantiate
these variables to `B b a` which is precisely equivalent to `Either b a`.

What this results in is that each free variable is a different type
from bound ones. `Scope` provides various functions for instantiating
bound variables and abstracting over free ones. That's `bound` in a
nutshell.

It's a bit easier to grok this by example, here's our calculus ported
to use `Scope`

``` haskell
    data Expr a = Var a
                | App (Expr a) (Expr a)
                | Annot (Expr a) (Expr a)
                | ETrue
                | EFalse
                | Bool
                | Star
                | Pi (Expr a) (Scope () Expr a)
                | Lam (Scope () Expr a)
                | C String
                deriving(Functor, Eq)
```

So the first major difference is that our polarization between
inferrable and checkable terms is gone! This wasn't something I was
happy about, but in order to use `Scope` we need a monad instance and
we can't define two mutually dependent monad instances without a
function from `CExpr -> IExpr`, something that clearly doesn't exist.

Since each binder can only bind one variable at a time, we represent
the newly bound variable as just `()`. This would be more complicated
if we supported patterns or something similar.

Now in addition to just this, we also need a bunch of boilerplate to
define some type class instances for `Scope`'s benefit.

``` haskell
    instance Eq1 Expr where (==#) = (==)
    instance Applicative Expr where
      pure = return
      (<*>) = ap
    instance Monad Expr where
      return = Var
      Var a >>= f = f a
      (App l r) >>= f = App (l >>= f) (r >>= f)
      ETrue >>= _ = ETrue
      EFalse >>= _ = EFalse
      Bool >>= _ = Bool
      Star >>= _ = Star
      C s >>= _ = C s
      Annot l r >>= f = Annot (l >>= f) (r >>= f)
      Pi l s >>= f = Pi (l >>= f) (s >>>= f)
      Lam e >>= f = Lam (e >>>= f)
```

That weird `>>>=` is just `>>=` that works through `Scope`s. It's a
little bit frustrating that we need this somewhat boilerplate-y monad
instance, but I think the results might be worth it.

From here we completely forgo an explicit `Val` type. We're completely
scrapping that whole HOAS and `VConst` ordeal. Instead we'll just
trust `Scope`'s clever `Eq` instance to handle alpha conversion. We do
need to implement normalization though

``` haskell
    type Val = Expr

    nf :: Expr a -> Val a
    nf = \case
      (Annot e t) -> nf e -- Important, nf'd data throws away annotations
      (Lam e) -> Lam (toScope . nf . fromScope $ e)
      (Pi l r) -> Pi (nf l) (toScope . nf . fromScope $ r)
      (App l r) ->
        case l of
         Lam f -> nf (instantiate1 r f)
         l' -> App l' (nf r)
      e -> e
```

What's interestingly different is actual work is shifted from within
the higher order binders we had before into the case expression in
`App`.

It's also worth mentioning the few bound specifics here. `toScope` and
`fromScope` expose the underlying `f (V b a)` that a `Scope` is
hiding. We're then can polymorphically recur (eat your heart out
sml) over the now unbound variables and continue on our way.

Again, notice that I've defined nothing to do with substitution or
scoping, this is all being handled by bound.

Now our actual type checker is still essentially identical. We're
still using `monad-gen` to generate unique variable names, it's just
that now `bound` handles the messy substitution. The lack of
distinction between inferrable, checkable, and normalized terms did
trip me up once our twice though.

``` haskell
    data Env = Env { localVars :: M.Map Int (Val Int)
                   , constants  :: M.Map String (Val Int) }
    type TyM = ReaderT Env (GenT Int Maybe)

    unbind :: (MonadGen a m, Functor m, Monad f) => Scope () f a -> m (a, f a)
    unbind scope = ((,) <*> flip instantiate1 scope . return) <$> gen

    unbindWith :: Monad f => a -> Scope () f a -> f a
    unbindWith = instantiate1 . return

    inferType :: Expr Int -> TyM (Val Int)
    inferType (Var i) = asks (M.lookup i . localVars) >>= maybe mzero return
    inferType (C s) = asks (M.lookup s . constants) >>= maybe mzero return
    inferType ETrue = return Bool
    inferType EFalse = return Bool
    inferType Bool = return Star
    inferType Star = return Star
    inferType (Lam _) = mzero -- We can only check lambdas
    inferType (Annot e ty) = do
      checkType ty Star
      let v = nf ty
      v <$ checkType e v
    inferType (App f a) = do
      ty <- inferType f
      case ty of
       Pi aTy body -> nf (App (Lam body) a) <$ checkType a aTy
       _ -> mzero
    inferType (Pi t s) = do
      checkType t Star
      (newVar, s') <- unbind s
      local (\e -> e{localVars = M.insert newVar (nf t) $ localVars e}) $
        Star <$ checkType s' Star

    checkType :: Expr Int -> Val Int -> TyM ()
    checkType (Lam s) (Pi t ts) = do
      (newVar, s') <- unbind s
      local (\e -> e{localVars = M.insert newVar (nf t) $ localVars e}) $
        checkType s' (nf $ unbindWith newVar ts)
    checkType e t = inferType e >>= guard . (== t)
```

I defined two helper functions `unbind` and `unbindWith` which both
ease the process of opening a scope and introducing a new free
variable. I actually split these off into a
[tiny library](bound-gen), but I haven't
uploaded it to hackage yet.

 1. Code size decreased by ~50 lines
 2. No more explicit substitution
 3. All the annoying plumbing is in the monad instance which is pretty
    mechanical
 4. We did lose the really nice separation of terms we had before
    though :(

I suppose that 4. would be a nonissue for a lot of people who don't
care about bidirectional type checkers.

## HOAS

Higher order abstract syntax is a really nifty trick. The idea is that
Haskell already has a perfectly good notion of variables and
substitution lying around! Let's just use that. We represent our
functions with actual `->`s and we don't have a constructor for
variables anymore.

The only issue is that Haskell doesn't let us inspect the bodies of
functions. We need to do this, however, for a type checker! To deal
with this we dirty our AST a bit and add in `IGen`'s, placeholders for
where normal Haskell variables would normally go. Our new AST looks
like this

``` haskell
    data Expr = App Expr Expr
              | Annot Expr Expr
              | ETrue
              | EFalse
              | Bool
              | Star
              | Pi Expr (Expr -> Expr)
              | Lam (Expr -> Expr)
              | C String
              | IGen Int

    type NF = Expr
```

Notice how both `Pi` and `Lam` have functions embedded in them. Now
normalization is actually quite slick because functions are easy to
work with in Haskell

``` haskell
    nf :: Expr -> NF
    nf ETrue = ETrue
    nf EFalse = EFalse
    nf Bool = Bool
    nf Star = Star
    nf (C s) = C s
    nf (IGen i) = IGen i
    nf (Annot l _) = nf l
    nf (Pi t f) = Pi (nf t) (nf . f)
    nf (Lam f) = Lam (nf . f)
    nf (App l r) = case nf l of
      Lam f -> nf . f $ l
      l' -> App l' (nf r)
```

This is actually quite similar to the `Val` type we started with. That
was also used HOAS and we end up with a similarly structured
normalization.

For the same reason, the equivalence checking procedure is pretty much
the same thing

``` haskell
    eqTerm :: NF -> NF -> Bool
    eqTerm l r = runGenWith (successor s) (IGen 0) $ go l r
      where s (IGen i) = IGen (i + 1)
            s _ = error "Impossible!"
            go Star Star = return True
            go Bool Bool = return True
            go ETrue ETrue = return True
            go EFalse EFalse = return True
            go (Annot l r) (Annot l' r') = (&&) <$> go l l' <*> go r r'
            go (App l r) (App l' r') = (&&) <$> go l l' <*> go r r'
            go (Pi t f) (Pi t' g) =
              (&&) <$> go t t' <*> (gen >>= \v -> go (f v) (g v))
            go (IGen i) (IGen j) = return (i == j)
            go _ _ = return False
```

In fact, the only differences are that

 1. There are a few more cases, even though they won't ever be called
 2. We don't need that horrible top level `Enum` instance

The only reason for two is that the amazing maintainer of `monad-gen`
(hi!) rejiggered some the library to not be so `Enum` dependent.

Now from here our type checker is basically what we had before. In the
interest of saving time, I'll highlight the interesting bits: the
constructors that bind variables.

``` haskell
    data Env = Env { localVars :: M.Map Int NF
                   , constants :: M.Map String NF }
    type TyM = GenT Int (ReaderT Env Maybe)

    inferType :: Expr -> TyM NF
    inferType (Pi t f) = do
      checkType t Star
      let t' = nf t
      i <- gen
      local (\e -> e{localVars = M.insert i t' $ localVars e}) $
        Star <$ checkType (f $ IGen i) Star

    checkType :: Expr -> NF -> TyM ()
    checkType (Lam f) (Pi t g) = do
      i <- gen
      let t' = nf t
          rTy = nf (g $ IGen i)
      local (\e -> e{localVars = M.insert i t' $ localVars e}) $
        checkType (f $ IGen i) rTy
```

At this point you may have started to notice the pattern, the only
real difference here is that substitution is completely
free. Otherwise, I don't really have much to say about HOAS.

## Wrap Up

In conclusion, I think we can all agree that the original version of
this type checker was unpleasant to say the least. It did considerably
improve with `bound` mostly because the normalize-and-compare
equivalence checking is really easy since `bound` handles alpha
conversion. On the other hand, actually doing work beneath a binder is
a bit of a pain since we have to take care to never unwrap a binder
with a previously bound variable. We handled this with a hacky little
trick with `monad-gen`, but a permanent and clean solution still seems
hard.

We can avoid this fully by hitching a ride on Haskell's variables and
substitution using HOAS, this is wonderful until it's not. The issue
is that comparing functions for equality is still a pain so we ended
up with an equivalence check much like what we had in the original
version.

In the future it'd be interesting to try this with `unbound`, a
library in the same domain as `bound` with a very different approach.

[original-tc]: /posts/2014-11-22-bidir.md
[bound-gen]: http://github.com/jozefg/bound-gen
[cooked-pi-github]: http://github.com/jozefg/cooked-pi
[cooked-pi-bitbucket]: http://bitbucket.org/jozefg/cooked-pi
