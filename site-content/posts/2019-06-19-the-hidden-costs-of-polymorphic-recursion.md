---
title: "Hidden costs of polymorphic recursions"
author: "Travis Athougies"
tags: "haskell"
published: true
---

Finger trees are a sequential, persistent data structure with very
good asymptotic bounds for common operations, such as concatenation,
adding at the beginning or end, and indexing / searching. Better yet,
they can be parameterized by measure types to form many useful data
structures, like priority queues, interval trees, key value arrays,
etc.

It's no surprise that they form the basis of several performant
Haskell packages such as the popular `Data.Seq` and `Data.FingerTree`.

A finger tree is basically a 2-3-4 tree turned on its head. The exact
layout of a finger tree is beyond the scope of this article, but a
good overview can be found
[here](http://www.staff.city.ac.uk/~ross/papers/FingerTree.html). Below
is a reproduction of a diagram of a finger tree from that paper.

Notice how at each level of the tree, subtrees are of the same
height. Most Haskell implementations of finger trees use a technique
known as *polymorphic recursion* to provide a compile time guarantee
that all child nodes at the same level have equal height. This is a
great technique, and Haskell is one of few languages that support
this.

However, while using finger trees in a recent project I was working
on, I began to notice that they allocated a lot more memory than I
expected. In garbage collected languages like Haskell, the larger size
of memory, the slower the program will run. Thus, I knew I needed to
minimize memory usage, while maintaining type safety. Luckily,
GHC's type system is expressive enough to make this change.

# The Structure

The most general finger tree structure (from the `fingertree` package)
is typically given as

```haskell
data FingerTree v a
    = Empty
    | Single
    | Deep v (Digit a) (FingerTree v (Node v a)) (Digit a)

data Digit a
    = One a
    | Two a a
    | Three a a a
    | Four a a a a

data Node v a = Node2 v a a | Node3 v a a a
```

On inspection, this seems pretty straight forward. I mean, most
Haskellers are used to recursive structures, such as the list.

```haskell
data [a] = [] | a : [a]
```

Notice the following subtlety with `FingerTree` though: whereas `[a]`
contains a field in the `(:)` constructor of type `[a]` (the same type
as the type we're defining), the `FingerTree v a` structure contains a
field in the `Deep` constructor of type `FingerTree v (Node v
a)`. That is to say, instead of a `FingerTree v a` `Deep` constructor
containing a finger tree holding elements of type `a`, the `Deep`
finger tree holds elements of type `Node v a`. Of course, `Node v a`
represents a tree with one level of nodes. The finger tree inside a
`Deep` constructor can of course also be another `Deep` instance, and
its sub tree will now contain elements of type `Node v (Node v a)` (a
two-level tree).

This matches the equal level property we desired above.

# The problem

On its own, this is fine. The issue, however, comes whenever we want
to operate on this tree. Take a look at the implementation of the
`traverse` function for `FingerTree`.

```haskell
traverse' :: (Measured v1 a1, Measured v2 a2, Applicative f) =>
    (a1 -> f a2) -> FingerTree v1 a1 -> f (FingerTree v2 a2)
traverse' = traverseTree

traverseTree :: (Measured v2 a2, Applicative f) =>
    (a1 -> f a2) -> FingerTree v1 a1 -> f (FingerTree v2 a2)
traverseTree _ Empty = pure Empty
traverseTree f (Single x) = Single <$> f x
traverseTree f (Deep _ pr m sf) =
    deep <$> traverseDigit f pr <*> traverseTree (traverseNode f) m <*> traverseDigit f sf

traverseNode :: (Measured v2 a2, Applicative f) =>
    (a1 -> f a2) -> Node v1 a1 -> f (Node v2 a2)
traverseNode f (Node2 _ a b) = node2 <$> f a <*> f b
traverseNode f (Node3 _ a b c) = node3 <$> f a <*> f b <*> f c

traverseDigit :: (Applicative f) => (a -> f b) -> Digit a -> f (Digit b)
traverseDigit f (One a) = One <$> f a
traverseDigit f (Two a b) = Two <$> f a <*> f b
traverseDigit f (Three a b c) = Three <$> f a <*> f b <*> f c
traverseDigit f (Four a b c d) = Four <$> f a <*> f b <*> f c <*> f d
```

In particular, notice the following case:

```haskell
traverseTree f (Deep _ pr m sf) =
    deep <$> traverseDigit f pr <*> traverseTree (traverseNode f) m <*> traverseDigit f sf
```

Because `(Deep _ pr m sf) :: FingerTree v a` but `m :: FingerTree v
(Node v a)`, notice how we cannot pass `f` directly to
`traverseTree`. Instead, we have to pass `traverseNode f`. This
seemingly innocuous line incurs an additional heap allocation (8 bytes
or more on a 64-bit architecture) with each level of the
tree.

Okay, so what you ask? All we're doing here is incurring an `O(log n)`
memory overhead when doing a traversal. That's not that big a deal in
a language like Haskell.

But wait, there's more...

The `traverseNode` function takes two arguments, but, when passed to
`traverseTree` it is only supplied one. That's fine by Haskell of
course. On the implementation level however, most Haskell
implementations only perform a reduction (a function call) when a
function is fully applied. Oftentimes, the compiler can cleverly
inline or reason its way out of having to examine application arity at
compile-time. Unfortunately, in the general case, this can't be done,
and -- as a rule of thumb -- GHC mainly gives up on inlining
optimizations when it comes down to recursive calls, like the one above.

This means that the incomplete application is stored on the heap as a
partial application (or PAP node in GHC's STG parlance).

But wait, there's more! Notice the `Measured` constraint on these
function calls. If GHC sees we have a `Measured v a` and needs to
construct a `Measured v (Node v a)` it's actually going to construct
yet another PAP node behind the scenes to handle the `measure`
function. This happens whether we like it or not, simply due to the
fact that `Measured` is listed as a constraint, and the `mesaure`
function is used (in this case by the call to `deep`).

So, aside from the O(log n) memory overhead, we also have a much more
involved arity check plus indirect function application. Ugh... this
is going to be really difficult to optimize.

# Indexing to the rescue

One key insight here is that the 'shape' of the nested types forces us
to perform these kinds of time-consuming allocations and function
calls. While technically correct, the code is not written in such a
way that the compiler can easily optimize, simply because modern
computer architectures are not really intended for this sort of highly
functional code.

On the other hand, GHC is great at optimizing tight loops. Nominally
of course, traverse should just be a tight loop, but, again, the
structure of the algebraic data type is getting in the way. Can we do
better?

One obvious solution is to simply get rid of the polymorphic
recursion. This has the advantage that we can now simply pass `f` to
each recursive call to `traverseTree` preventing the naughty
allocation.

However, for the type-minded in the audience, this solution doesn't
sit well. The point of the recursion was to limit the kinds of data
structures that are members of this type to only the finger tree
structures that are balanced. Free, unbounded recursion of the fingers
would lose that guarantee.

# An alternative

What about, instead of explicit polymorphic recursion, we instead
parameterized each `FingerTree` by its depth in the top-level finger
tree. If we had the depth available to us, then we would know how many
levels the fingers must have.

First, our level type, a simple type-level natural number

```haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
data Level = LZ | LSucc Level
```

Now, a finger tree, paramaterized by level

```haskell
data FingerTreeN (l :: Level) v a where
   Empty :: FingerTreeN l v a
   Single :: Tree l v a -> FingerTreeN l v a
   Deep :: v -> Digit l v a -> FingerTreeN ('LSucc l) v a -> Digit l v a -> FingerTreeN l v a

data Digit (n :: Level) v a
  = One   (Tree n v a)
  | Two   (Tree n v a) (Tree n v a)
  | Three (Tree n v a) (Tree n v a) (Tree n v a)
  | Four  (Tree n v a) (Tree n v a) (Tree n v a) (Tree n v a)

data Node n v a
  = Node2 !v !(Tree n v a) !(Tree n v a)
  | Node3 !v !(Tree n v a) !(Tree n v a) !(Tree n v a)

newtype Tree n v a = Tree (TreeF n v a)

type family TreeF (n :: Level) v a where
  TreeF LZ v a = a
  TreeF (LSucc l) v a = Node l v a
```

Notice now that the recursive embedding of `FingerTreeN` is still
applied at `a`. All the polymorphism has been 'lifted' into the
type-level successor index of the `FingerTreeN` data family.

Now all we need to note is that the top-level `FingerTree` is a
`FingerTreeN` applied at level `LZ`.

```haskell
newtype FingerTree v a = FingerTree (FingerTreeN 'LZ v a)
```

Interestingly enough, pretty much all the implementation can stay the
same, with one caveat.

The `Level` parameter to the type has to be reified at the value level
in order to be of use. This means that we need to pass an extra
element to our functions to have them be of any use. We define a new
singleton for our `Level` type.

```haskell
data RuntimeDepth (l :: Level) where
  RuntimeZ :: RuntimeDepth 'LZ
  RuntimeSucc :: RuntimeDepth l -> RuntimeDepth ('LSucc l)
```

Ignore the various fields on the constructors for now.

Now, we can pattern match on `RuntimeDepth` to prove to the compiler
if `TreeF n v a` is `a` or an application of `Node`.

``` haskell
traverse' :: (Measured v1 a1, Measured v2 a2, Applicative f)
          => (a1 -> f a2) -> FingerTree v1 a1 -> f (FingerTree v2 a2)
traverse' f (FingerTree t) = FingerTree <$> traverse'' zeroDepth f t

traverse'' :: (Measured v1 a1, Measured v2 a2, Applicative f)
           => RuntimeDepth l -> (a1 -> f a2) -> FingerTreeN l v1 a1 -> f (FingerTreeN l v2 a2)
traverse'' !_ _ Empty = pure Empty
traverse'' !l f (Single x) = Single <$> traverseTree l f x
traverse'' !l f (Deep _ pr m sf) = deep l <$> traverseDigit l f pr <*> traverse'' (nextDepth l) f m <*> traverseDigit l f sf

traverseTree :: (Measured v1 a1, Measured v2 a2, Applicative f)
             => RuntimeDepth l -> (a1 -> f a2) -> Tree l v1 a1 -> f (Tree l v2 a2)
traverseTree (RuntimeZ _) f (Tree x) = Tree <$> f x
traverseTree (RuntimeSucc l') f (Tree (Node2 _ a b)) = treeDigit <$> (node2 l' <$> traverseTree l' f a <*> traverseTree l' f b)
traverseTree (RuntimeSucc l') f (Tree (Node3 _ a b c)) = treeDigit <$> (node3 l' <$> traverseTree l' f a <*> traverseTree l' f b <*> traverseTree l' f c)

traverseDigit :: (Measured v1 a1, Measured v2 a2, Applicative f)
              => RuntimeDepth l -> (a1 -> f a2) -> Digit l v1 a1 -> f (Digit l v2 a2)
traverseDigit !l f (One a) = One <$> traverseTree l f a
traverseDigit !l f (Two a b) = Two <$> traverseTree l f a <*> traverseTree l f b
traverseDigit !l f (Three a b c) = Three <$> traverseTree l f a <*> traverseTree l f b <*> traverseTree l f c
traverseDigit !l f (Four a b c d) = Four <$> traverseTree l f a <*> traverseTree l f b <*> traverseTree l f c <*> traverseTree l f d

nextDepth :: RuntimeDepth l -> RuntimeDepth ('LSucc l)
nextDepth = RuntimeSucc
```

Of course, if we had to continuously construct new `RuntimeDepth`s
using `nextDepth`, we're still allocating things on the heap. Luckily,
we can get rid of this too, by adding a pointer to each `RuntimeDepth`
constructor to the next one in the series.

```haskell
data RuntimeDepth (l :: Level) where
  RuntimeZ :: RuntimeDepth ('LSucc 'LZ) -> RuntimeDepth 'LZ
  RuntimeSucc :: RuntimeDepth ('LSucc ('LSucc l)) -> RuntimeDepth l -> RuntimeDepth ('LSucc l)

zeroDepth :: RuntimeDepth 'LZ
zeroDepth = let x = RuntimeZ (next x)

                next prev = let y = RuntimeSucc prev (next y)
                            in y
            in x

nextDepth :: RuntimeDepth l -> RuntimeDepth ('LSucc l)
nextDepth (RuntimeZ x) = x
nextDepth (RuntimeSucc x _) = x
```

Now, due to the power of laziness, these are only ever allocated once
per run of the program. Once allocated, the nodes are reused. We now
have allocation free recursion for `traverse`!

Let's see if this stands to scrutiny.

# Benchmarking

My version of finger trees (with code broadly copied from the
`fingertree` package) is available on
[Hackage](https://hackage.haskell.org/pinky). I've named these
structures 'pinky trees' because they're like finger trees, but
lighter weight.

I'm going to use the benchmarks from the haskell-perf `sequences`
[repository](https://github.com/haskell-perf/sequences). We're going
to look at total runtime as well as memory usage.

I'm going to compare my trees against the generic `fingertree` package
to make a fair comparison (`Data.Sequence` explicitly fixes the `v`
above allowing the compiler to make some heap representation
optimizations not available in the general case).

My benchmarks are available [here](http://github.com/tathougies/pinky-tree-bench).

As you can see, pinky trees are superior in all respects to finger trees.

# Next steps

The `Data.FingerTree.Pinky` package is a pretty much drop-in
replacement for `Data.FingerTree`. There are a few missing functions,
but they should be easily added by copy-pasting (a technique I rarely
recommend).

Happy finger tree-ing!
