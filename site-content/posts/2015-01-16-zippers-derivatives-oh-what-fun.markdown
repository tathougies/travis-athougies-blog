---
title: Zippers, Derivatives -- Oh what fun!
author: Travis Athougies
tags: haskell, algebra,types
---

Haskell's datatypes are *algebraic*. That is, every type can be represented as an addition or
multiplication of other types (or any combination of sums and products of other types).

This means that we can use all standard algebraic operations on them. For example, taking the sum of
two types is the same as using the `Either` type since the new value can be of the first type or of
the second type. Similarly, taking the product is the same as using the `(,)` constructor since it
requires both the left type and the right type.

Things get more interesting however when you apply more complicated mathematical operations to these
types. For example, as shown in
[this paper](http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.22.8611) and as explained on
[this website](http://chris-taylor.github.io/blog/2013/02/13/the-algebra-of-algebraic-data-types-part-iii/),
if you *differentiate* the algebraic representation of a data type you will get the corresponding
zipper type! That's right, you can *differentiate* Haskell data types and get a very useful result!

Before you continue reading this article, make sure you have a good understanding of what zippers
are. The last chapter of [Learn You a Haskell](http://learnyouahaskell.com/zippers) is a great
resource.

So great, we now have a general procedure for calculating the zipper of any data type. As an
example, let's create a zipper for the following type, a "Rose tree." Rose trees are like a binary
tree except they can have many branches.
```haskell
data Rose a = Rose a [Rose a]
```
First, let's represent the structure algebraically. We'll say
\\[ R(a) = a \\times L(R(a)), \\]
where $L(a)$ is the represents the type of lists of type $a.$ We can write $L(a)$ as
\\[ L(a) = 1 + a \\times L(a) ,\\]
or, if we rearrange and solve for $L(a),$
\\[ L(a) = \\frac {1}{1 - a} .\\]

Let's find a zipper for Rose trees!
\\[ R'(a) = a (L(R(a)))' + L(R(a)) a' = a L'(R(a))R'(a) + L(R(a)) .\\]
Now, as stated in the articles above, the zipper for a list is simply the product of two lists. That
is,
\\[L'(a) = L(a)^2.\\]
Therefore,
\\[ R'(a) = a L(R(a))^2R'(a) + L(R(a)). \\]

Now, rearranging terms,
\\[ R'(a) - a L(R(a))^2R'(a) = L(R(a)). \\]
\\[ R'(a) (1 - a L(R(a))^2) = L(R(a)). \\]
\\[ R'(a) = L(R(a)) \\left(\\frac{1}{1 - a L(R(a))^2}\\right) .\\]

Recall that the type of lists $L(a)$
\\[L(a) = \\frac{1}{1 - a} .\\]
Therefore,
\\[\\frac{1}{1 - aL(R(a))^2} = L(aL(R(a))^2) .\\]

Now, we can write the final derivative of `Rose a`.
\\[R'(a) = L(R(a)) L(aL(R(a))^2) .\\]

Writing this as Haskell syntax, our new type `DRose a` is the product of two lists, one of `Rose a`
and the other of `(a,[Rose a],[Rose a])`.
```haskell
data DRose a = DRose [Rose a] [(a, [Rose a], [Rose a])]
```
*To be continued...*
