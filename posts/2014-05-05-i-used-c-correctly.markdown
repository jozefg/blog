---
title: Getting Proper Tail Calls Out of C
---

While I don't exactly love *writing* C, it has
a lot to offer as a compilation target. Its got lots
of smart compilers that can target just about every platform
I've ever heard of and tons of others, its got the ability
to mess with low level aspects of itself, and C's got some nice
high level abstractions like functions.

One big issue I have with it as a target is that its function calls suck.
I'm usually compiling a functional language where tail call optimization is
imperative (heh) and C makes this a lot harder than it should.

This post illustrates how I currently beat C into actually generating proper
tail calls. Most of the code from this post is straight from [c_of_scheme](http://www.bitbucket.org/jozefg/c_of_scheme). If
you're having trouble understanding some function than there may actually be documentation for
it in the source :)

The first step involves something called continuation passing style. The idea here
is that reify the implicit "continuation" for each expression to an explicitly function.

So we'll turn something like

``` scheme
    (+ 1 (* 2 2))
```

Into something like

``` scheme
    (lambda (k)
       ((lambda (mult-result)
          (k (+ 1 mult-result)))
         (* 2 2)))
```

Notice how now the order of evaluation is completely determined by how we pass things around?
We pass each result along the chain of continuations and every non-primitive function becomes
a tail call.

This has one more very important effect, none of these function calls will return. We're going
to pass control off to each continuation and the very last function will exit the program. This
means that as soon as we call a continuation, we can nuke the stack and every function call has
become identical to calling a continuation.

C actually has a similar notion to this and when we run this code through closure conversion
and lambda lifting (a subject that's worth of its own rant) we'll end up with functions that look
something like

``` c
    void _gen1(scm_t arg, scm_t cont, ...){
       scm_apply(cont, scm_add(scm_t arg, 1));
    }
```

It's worth a mention that `scm_apply` will unwrap the continuation and actually apply it since it's
just a normal function. We know that the call to `scm_apply` will never return. We can tell
C this with `__attribute__((noreturn))`. Theoretically this also enables the
use of something much like TCO: once the last function is called, we can reuse
the stack frame of `_gen1` and if the function actually returns despite our promises
simply segfault (hooray for C).

Unfortunately, GCC doesn't seem to do this on its own in my case. So I cried for a little
bit and offered it many flags in the hopes that it would be merciful and just do it for
me but it didn't. And now I can actually illustrate how I did this manually.

It turns out this is possible to do with only a tiny impact on the generated code from
the compiler and a bit of monkeying with `scm_apply`. First, I'll explain how `scm_apply`
looks normally.

``` c
    void scm_apply(int i, scm_t f, ...) {
      int x;
      va_list va;
      scm_t *arg_list = malloc(sizeof(scm_t) * i + 1);
      va_start(va, f);
      for(x = 1; x < i+1; ++x){
        arg_list[x] = va_arg(va, scm_t);
      }
      if(f->state != 4){
        printf("Attempted to apply nonfunction\n");
        exit(1);
      } else {
        arg_list[0] = f->val.scm_lam.clos;
        f->val.scm_lam.fun(arg_list);
      }
    }
```

Note that `scm_t` is a pointer to a discriminated union in C
to fake the dynamic types found in Scheme.

So the first bit is just the varargs goo to extract the
arguments given to `scm_apply`. Once we have all of those
in an array, we look at the `state` field of `f`, our function.
If it's not 4, then we don't really have a function so we complain
loudly and exit.
Otherwise we just get the actual function pointer out of `f` and call it.

This is a little tricky to read if you're not familiar with the DU's in C,
but there's nothing exactly earth shattering in there.

Now, since every function call is going through `scm_apply`, we add a global
ticker to count how many function calls have gone through there

``` c
    static int stack_frames;
    ...
    void scm_apply(int i, scm_t f, ...) {
        ...
        else {
            ++stack_frames;
            ....
        }
    }
```

Now we know just *how* quickly we're burning through the available stack space.

Next we need to add a special case of `scm_apply` which we'll call `scm_init`.
It looks like this

``` C
    void scm_init(lam_t f){
       stack_frames = 0;
       scm_apply(0, mkLam(scm_top_clos, f)); // Call main
    }
```

All this does is initialize `stack_frames` and call `scm_apply`. We can modify
the codegen so that the `main` function is passed to `scm_init`. We know that
this main function will take no arguments in `c_of_scheme` for reasons that aren't entirely
relevant to this post.

OK, so now is the magic and like all good C magic, it starts by including `setjmp`.

``` c
    #include <setjmp.h>
```

Now we add 3 more global variables (please don't hate me)

``` c
    static scm_t  current_fun;
    static scm_t* current_args;
    static jmp_buf env;
```

Now we modify `scm_apply` so that if we're at a depth of 100 function calls or more
we stick the current function and arguments into these global variables and `longjmp` with
`env`!

Now we need a good place to `longjmp` to, the place where `env` points to. This is what `scm_init`,
we know that it's called almost immediately so it's relatively "low" on the stack. So `scm_init`
now becomes

``` c
    void scm_init(lam_t f){
      stack_frames = 0;

      if(setjmp(env)){
         stack_frames = 0;
         current_fun->val.scm_lam.fun(current_args);
      }
      scm_apply(0, mkLam(scm_top_clos, f)); // Call main
    }
```

Notice that we do know error checking and just go straight into calling the next function
after a `longjmp`. In order to set up `current_fun` and `current_args` correctly `scm_apply`
must be modified

``` c
    void scm_apply(int i, scm_t f, ...) {
      int x;
      va_list va;
      scm_t *arg_list = malloc(sizeof(scm_t) * i + 1);
      va_start(va, f);
      for(x = 1; x < i+1; ++x){
        arg_list[x] = va_arg(va, scm_t);
      }
      if(f->state != 4){
        printf("Attempted to apply nonfunction\n");
        exit(1);
      } else {
        arg_list[0] = f->val.scm_lam.clos;
    
        if(stack_frames >= 100){
          // Transfer continuation up
          current_fun     = f;
          current_args    = arg_list;
          longjmp(env, 1);
        }
        ++stack_frames;
        f->val.scm_lam.fun(arg_list);
      }
    }
```

This meant that now when we've applied 100 functions, we jump back to `scm_init`, demolishing
all those unused stack frames and keep going.

There it is, that's my minimally invasive technique for tail calls in C. From what I've
heard this is also used by Chicken Scheme.
