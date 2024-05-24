---
title: In Search of a Small Language for Homotopy Type Theory
author: Travis Athougies
---

Homotopy type theory is the 'latest' in a series of improvements over
the simply typed lambda calculus. Ambitious in its efforts to serve as
a foundation for the entirety of mathematics, there's increasing
speculation that it could perhaps serve as the basis of a dependently
typed functional language as well.

In this blog post, I'm exploring the design space of a minimalist
language for homotopy types. Specifically, I'd like a core low-level
language that a more user-facing language can compile too.

# Homotopy Type Theory

In Homotopy Type Theory, every object worthy of discourse is an
element (called a *point*) of some type (called a *space*). In
addition to points, types also have equalities between points, also
called *paths*. Every point gives rise to an equality with itself
(often label `refl`).

Homotopy type theory axiomatically defines equality as reflexive,
symmetric, and transitive.

A path between two points `a` and `b` of a space `A` has type
`a =b`. If `a` and `b` are identical, then one inhabitant (again, a
*point*) of this type (again, a *space*) is `refl`. However, there
could be any number of paths between `a` and `b` and it is foreseeable
that these paths even require computation to construuct.

Either way, homotopy type theory contains one other axiom. If you have
a point of `x : a = b` and a point `y : a = b`, then `x = y`. That is
to say, for any `a, b: A`, `(a = b) = (a = b)`. This axiom is known as
univalence.

# Constructing types

Types can be thought of as collections of elements. Of course,
allowing any collection of elements satisfying any predicate arrives
at a contradiction, similar to set theory. Instead, homotopy type
theory provides explicit means to construct types as sums and products
of the fundamental types, which include a unit type and the type of
functions.


In our type theory, we have a notion of primitive types. These types
are the types from which all other types are built. To specify each
type, we will first describe how we notate that type, how we form
objects of that type, how we eliminate objects of that type, how
eliminators work on constructors, and any rules for canonical forms

In our type theory we have the following types.

1. Dependent function type (pi-types)
2. Dependent pair type (sigma-types)
3. The boolean type (2)

These are sufficient to notate all other types we could want.

Note that, internally, and as an implementation detail,
implementations of this type theory could contain more primitive types
(such as machine words), and transform algorithms appropriately to
these types via homotopy.

## Dependent function types

The dependent function type is notated

forall (x : A). B

x may occur freely in B

It is formed via a lambda-abstraction

fun (x : A). B

It is eliminated via application.

(fun (x : A). B) a => B[x / a]

where '=>' stands for 'reduces to'. This transformation is called beta-reduction.


Finally, one more rule for uniqueness (called eta-reduction)

fun (x : A). (fun (x : A). B) x == fun (x : A). B

or fun(x : A). f x == f

Here are the typing rules

```
O, x : A |- b : B
------------------
O |- fun (x : A). b : forall (x : A). B
```


## Dependent pair types

A dependent pair type consists of one value of a constant type along
with another value whose type depends (hence dependent typing) on the
first value.

We write a dependent type

exists (x : A). B

x may occur freely within B.

We can form a member of this type:

given a. b

we can eliminate.

inspect C (given a. b) (fun (x : A). fun (b : B). c) : C

```
O |- p : exists (x : A). B
O, x : A |- b : B
O, x : A, b : B |- c : C
---
O |- inspect C p (fun (x : A). fun (b : B). c) : C
```

## Boolean type

0 : 2 and 1 : 2

## Void type

The void type is noted _|_.

## Unit type is simply


