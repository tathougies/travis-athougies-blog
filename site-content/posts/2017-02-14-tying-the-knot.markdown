---
title: "Tying the knot: persistent graphs"
author: Travis Athougies
tags: "haskell"
published: false
---

Haskell data structures are *persistent*. That means you cannot update the
fields of a record 'in-place'. Instead, you modify a record by constructing a
*new* version of that record with a field changed. This works well for common
data structures such as simple sum or record types or nested data types. It also
works very well for recursive data structures, such as lists, trees, sequences,
tries, and most of the other 'container' data structures we're all familiar
with.

Under the hood, these data structures are represented as pointers in the same
manner you would expect in a more 'traditional' language like C. However, since
Haskell will never update a pointer in-place, inserting a node into a tree
usually involves reconstructing the *spine* of the data structure, as
illustrated below, where the spine of the updated node is illustrated in
red. For lists, modifying the *n*th element involves constructing the entirety
of the list up to that element.

Notice that in both our examples of updating trees and lists, we end up with
pointers to two trees and two lists -- a new version and an old version. This
behavior is very convenient for reasoning and concurrency purposes, and, for the
most part, GHC can perform these updates efficiently.

Notice also that for both trees and lists, it is indeed possible to construct a
new structure by only updating nodes along the path between the root and the
target node. Not all data structures have this property. For example, graphs are
a common data structure that do not meet this property.

Let's see what we mean by this.

## Constructing a graph

Suppose you had the following graph, where each node is of type `GridPoint`,
defined by the ADT:

```haskell
data GridPoint x
    = GridPoint
    { north, east, south, west :: Maybe GridPoint
    , payload :: x }
    deriving Show
```

In an imperative language, you can construct such a graph by relying on
mutation. For example in python you might do

```python
# Construct points using whatever neighbors we have available
a = GridPoint()
b = GridPoint(west=a)
c = GridPoint(west=b)
d = GridPoint(north=a)
e = GridPoint(north=b,west=d)
f = GridPoint(north=c,west=e)
g = GridPoint(north=d)
h = GridPoint(north=e,west=g)
i = GridPoint(north=f,west=h)

# Fix up pointers
a.south = d
a.east = b
b.south = e
b.east = c
c.south = f
...
```

Luckily, in Haskell, we can construct this graph using very straightforward
syntax

```haskell
let a = GridPoint Nothing  (Just b) (Just d) Nothing  100
    b = GridPoint Nothing  (Just c) (Just e) (Just a) 200
    c = GridPoint Nothing  Nothing  (Just f) (Just b) 300
    d = GridPoint (Just a) (Just e) (Just g) Nothing  400
    e = GridPoint (Just b) (Just f) (Just h) (Just d) 500
    f = Gridpoint (Just c) Nothing  (Just i) (Just e) 600
    g = GridPoint (Just d) (Just h) Nothing  Nothing  700
    h = GridPoint (Just e) (Just i) Nothing  (Just g) 800
    i = GridPoint (Just f) Nothing  Nothing  (Just h) 900
```

Notice that the definition for `a` relies on the value of `b` and `d`, which
haven't been defined yet at the syntactic location of `a`. We can do this in
Haskell and not in Python because Haskell is a lazy language.

However, there are a few things that make me sad about this definition. Firstly,
using typical Haskell functions on it may result in infinite recursion. For
example, if you used the standard `deriving Show` instance for `GridPoint` and
then tried to evaluate `show a`, it will quickly go into an infinite loop as it
attempts to continuously reprint `a` as the `west` field of `b`.

Secondly, this 'breaks' record update syntax. For example, suppose you have a
reference to `a` and now want to construct a new graph where the vaue of `b`
is 201. You might think it is as easy as the following

```haskell
let a' = a { north = north a { payload = 201 } }
```

However, as the diagram below illustrates, the value pointed to by `a'` now
points into a structure that still contains links to the old `a`! This is not
the behavior we would expect, but it is the behavior we are forced to tolerate
in a language with persistent data structures -- Haskell simply cannot mutate
the inner links. We may hope that the language will eventually 'figure this
out', but alas, this kind of reasoning about code is impossible to perform in an
automated and terminating way.


## Current approaches

It seems we've hit a conundrum -- a data structure that seems impossible to
update in the way Haskell wants us to. So how are graphs represented in Haskell
today?

The most common approach is to use a library like `fgl`, which provides an
abstract interface to graphs. `fgl` is based on the concept of a `Context` which
includes a node, its label, and all labeled edges into and out of the node. It
treats graphs abstractly as algebraic combinations of these `Context`s. Graph
consumption is achieved via a `matchAny` function, which, given a graph,
extracts an arbitrary `Context` and returns that `Context` plus a new graph
representing the original graph sans the `Context`.

Under the hood, the graphs are represented in their mathematical definition as a
list of nodes and a list of edges. Each node is assigned a unique integer id,
and mapping is kept between ids and nodes. Thus, if we were to use FGL for our
grid point data above, we would have to redefine our `GridPoint` structure to
get rid of the explicit `north`, `east`, `south`, and `west` pointers:


```haskell
data GridPointFGL x
    = GridPointFGL
    { payloadFgl :: x }
```

Now we can construct a representation of this graph by combining `Context`s:

```haskell
let fglGraph = ([], 100, 0, [])                          & -- A
               ([((), 0 {-# ID for A #-} )], 200, 1, [((), 0)]) & --B
               ...
```

We've lost quite a bit of data with this approach. For example, we've lost any
sort of sense of which direction each edge is leading. Consider, the context of
B in the final graph.

```
let bContext = ([((), aId), ((), cId), ((), eId)], 200, 1, [((), aId), ((), cId), ((), eId)])
```

Notice that we've completely lost all information on whether `a` is to the
north, south, east, or west of us. Certainly, we could use edge labels to get
this information, but we would still be forced to lookup the correct label in
the adjacency lists.

The bindings in fgl seem a bit 'lose', and it seems like we've lost a lot of the
advantages of having a type system -- being forced to look up values in a list
is akin to accessing JavaScript object fields, and we know where that kind of
thinking leads us.. Indeed, it's possible to construct an invalid graph using
the functions available in fgl library -- a graph with edges between existing
and non-existing nodes or even between two non-existing nodes.

## Can we do better?

Let's try to do better. Instead of thinking about a potential solution, let's
approach the problem by attempting to write a solution, pretending we are
working in a language that could just do everything we wanted.

```haskell
modifyAsNeighbor :: GridPoint -> GridPoint
modifyAsNeighbor a =
  let a' = a { north = fixUpAllReferencesToA a a' (north a { payload = 201 })
             , south = fixUpAllReferencesToA a a' (south a)
             , east = fixUpAllReferencesToA a a' (east a)
             , west = fixUpAllReferencesToA a a' (west a) }
  in a'
```

In the above example the `fixUpAllReferencesToA` function takes the old `a` and
the new `a` and returns its third argument, but with all references to the old
`a` replaced by the new `a'`. Lets write this function.

```haskell
fixUpAllReferencesToA :: GridPoint -> GridPoint -> GridPoint
fixUpAllReferencesToA a a' x =
    x { north = if north x == a then a' else a
      , ... }
```

Wow! Syntactically, that seems to work, and semantically it does as
well. However, remember what we said above about the default `Show` instance no
longer terminating for `a`? Unfortunately, the same can be said for `Eq`. Thus,
the best that can be said about the above piece of code is that after Haskell
has verified that `north x` is the same as `a`, it will replace `north x` with
`a'`. While true and offering what we want, this is an empty promise.

What we really want is an operator `==*` that return `True` if and only if its
two arguments *point* to the same value, regardless of whether or not their
value is equal.

Unfortunately, since Haskell objects are referentially transparent, such an
operator is impossible to construct outside of `IO`, and even there, such an
operator can't be constructed with many guarantees.

## Stepping back

Let's try approaching our problem by taking a step back and examining what it
means for a data type to refer to itself, since it seems that's where our
problem stems from. In fact, it seems like our problem would simply not exist in
a language that didn't have recursive data structures.

This is a great place to start, so let's start by making `GridPoint`
non-recursive. We can do this for every Haskell data type with a simple
transformation:

```haskell
data GridPointF x f
    = GridPointF
    { north, east, south, west :: Maybe f
    , payload :: x }
```

Notice that we removed all recursive references to `GridPoint` and instead added
a new type parameter that we put in its place.

We can get back our original `GridPoint` by parameterizing `GridPointF` in a
special way.

```haskell
type GridPoint x = GridPointF x (Fix (GridPointF x))
```

