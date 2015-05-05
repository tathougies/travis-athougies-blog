---
title: Fun with Haskell
tags: haskell
author: Travis Athougies
published: false
---

As I was working on some projects in Haskell (a new Haskell web framework to be exact), I stumbled
across a seemingly impossible Haskell problem. I wanted to write a function, let's call it
`monadifyFirstArg` that took any function of the form

```haskell
f :: Monad m => a1 -> a2 -> ... -> an -> m r
```

and converted it into one of the form

```haskell
f :: Monad m => m a1 -> a2 -> ... -> an -> m r
```

Instead of taking in the parameter `a1` directly, this new function would take a monadic action that
returns this parameter (let's call it `mkA1`) , and then the rest of the arguments. When the
resulting monadic action is invoked, the result would first run `mkA1` and then pass the output to
the original `f`, before passing in all the remaining arguments.

This seems like a pretty easy function to write, because a human could do it quite simply. If I
asked you to write this function, for some `f`, you would be able to rattle it off:

```haskell
f :: Monad m => a1 -> a2 -> ... -> an -> m r
monadifiedF :: Monad m => m a1 -> a2 -> ... -> an -> m r

monadifiedF mkA1 a2 ... an= do a1 <- mkA1
                               f a1 a2 ... an
```

But, in this problem, we want the compiler to be able to do this for us. Namely, we want to write a
function `monadifyFirstArg` such that `monadifyFirstArg f = monadifiedF` (this isn't a formal
semantic, but just something to get our thinking juices flowing). What would the type of this
function be? Intuitively, it should be something like

```haskell
monadifyFirstArg :: (a1 -> .... -> m r) -> m a1 -> ... -> m r
```

Needless to say, however, this isn't valid Haskell.

What do we do now? Let's try to write out the idealized type signature fully parenthesized

```haskell
monadifyFirstArg :: (a1 -> (a2 -> (... (an -> mr)))) -> (a2 -> (... (an -> mr)))
```

Aha! This sheds some light on the situation.If you notice, the two parenthesized expressions (`(a2
-> (... (an -> mr)))`) are the result of functions (the function passed in as the first parameter
and the output of the `monadifyFirstArg` function). This means that the entire type can be replaced
by a type variable. Let's call this type variable `o`. Now, we can write

```haskell
monadifyFirstArg: (a1 -> o) -> m a1 -> o
```

But this loses some meaning. Namely, we want to constrain `o` to only being types of the form `a2 ->
... -> m r`. What's the standard Haskell solution to constraining types to a certain form? You
guessed it, type classes!

> ~~LARGE~~
> What's the standard Haskell solution to constraining types for a certain form? You guessed it,
> ***type classes!***

```haskell
class ResultsInMonad o

instance Monad m => ResultsInMonad (m o)
instance ResultsInMonad b => ResultsInMonad (a -> b)

monadifyFirstArg :: ResultsInMonad o => (a1 -> o) -> m a1 -> o
```

Great! By constraining `o` to be a member of the `ResultsInMonad` class, we can now place
restrictions on the form of its type. We create two instances of `ResultsInMonad`. The first
instance `Monad m => ResultsInMonad (m o)` means that any pure monadic action results in a monad. This
allows the Haskell compiler to verify that `monadifyFirstArg :: (a1 -> m r) -> m a1 -> m r` is a
valid. The sceond instance says that any function whose result eventually results in a monad is also
an instance of `ResultsInMonad`. So far, so good!

There is only one problem however. The monad `m` in the `ResultsInMonad (m o)` can be any monad
whatsoever. That is, `IO a` is a valid instance and so is `ST a` and `Maybe a` and `[a]`. This means
that the following is a valid type for `monadifyFirstArg`

```haskell
monadifyFirstArg :: (a1 -> a2 -> Maybe r) -> IO a1 -> a2 -> Maybe r
```

Uh oh! This isn't what we wanted, we need some way to constrain that the monad in question is a
certain type. We can do this by adding a parameter to the `ResultsinMonad` class (and enable a few
GHC extensions)

```haskell
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, KindSignatures #-}

class ResultsInMonad o m

instance Monad m => ResultsInMonad (m o) m
instance ResultsInMonad b m => ResultsInMonad (a -> b) m

monadifyFirstArg :: ResultsInMonad o m => (a1 -> o) -> m a1 -> o
```
