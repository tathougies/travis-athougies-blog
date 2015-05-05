---
title: "Zippers, Derivatives -- Oh what fun!"
author: Travis Athougies
tags: "haskell, math"
published: true
---

Haskell's datatypes are *algebraic*. That is, every type can be represented as an addition or
multiplication of other types (or any combination of sums and products of other types).

This means that we can use all standard algebraic operations on them. For example, taking the sum of
two types is the same as using the `Either` type since the new value can be of the first type _or_ of
the second type. Similarly, taking the product is the same as using the `(,)` constructor since it
requires both data from both the left type and the right type.

Things get more interesting however when you apply more complicated mathematical operations to these
types. For example, as shown in
[this paper](http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.22.8611) and as explained on
[this website](http://chris-taylor.github.io/blog/2013/02/13/the-algebra-of-algebraic-data-types-part-iii/),
if you *differentiate* the algebraic representation of a data type you will get the corresponding
zipper type! That's right, you can *differentiate* Haskell data types and get a very useful result!

Before you continue reading this article, make sure you have a good understanding of what zippers
are. The last chapter of [Learn You a Haskell](http://learnyouahaskell.com/zippers) is a great
resource.

## An Example

Great! We now have a general procedure for calculating the zipper of any data type. As an
example, let's create a zipper for the following type, a "Rose tree." Rose trees are like a binary
tree except they can have many branches.
```haskell
data Rose a = Rose a [Rose a]
```
First, let's represent the structure algebraically. The product has one constructor so there will be
no sum terms. The only constructor takes two arguments, so the algebraic type will be the product of
these two types. We'll say
\\[ R(a) = a \\times L(R(a)), \\]
where $L(x)$ is the type of lists holding values of type $x.$

We can write $L(a)$ as
\\[ L(a) = 1 + a \\times L(a) ,\\]
or, if we rearrange and solve for $L(a),$
\\[ L(a) = \\frac {1}{1 - a} .\\]
You may be asking: *How could he re-arrange terms like that? We never spoke about subtraction and
division of types!* Indeed, you would be right. In most semirings (the general name for mathematical
objects that act like algebraic data types), you would not be able to make such statements. However,
in the case of types, we can use both subtraction and division in our proofs, and we're guaranteed
that, although we can't really divide or subtract types, there is a way to make the same proof
without using these operations. For more information, see
[here](http://arxiv.org/pdf/math/9405205v1.pdf).

Now, let's find a zipper for Rose trees!
\\[ R'(a) = a (L(R(a)))' + L(R(a)) a' = a L'(R(a))R'(a) + L(R(a)) .\\]
Now, as stated in the articles above, the zipper for a list is simply the product of two lists. That
is,
\\[L'(a) = L(a)^2.\\]
Therefore,
\\[ R'(a) = a L(R(a))^2R'(a) + L(R(a)). \\]

Rearranging terms,
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

### Interpretating the result

Although the differentiation procedure has given us a new type. It hasn't told us anything about how
the new type should be interpreted. After all, what does the first `[Rose a]` represent? If we think
about it though, we can see pretty clearly that the type `DRose a` corresponds to a position in the
rose tree `Rose a`, where our children are `[Rose a]` and `[(a, [Rose a], [Rose a])]` is the list of
parent contexts, where `a` is the value of the node and `[Rose a], [Rose a]` is a list zipper into
the list of children.

## Building an intuition

Can we explain easily why these rules work? Below I try to build an intuition, but I won't be very
rigorous &em; see McBride's paper for that.

Let's start with the simplest derivative. The zipper of a type with respect to itself. In this case,
there is only one possible place that a "hole" for the type could be. Therefore, the type of this
zipper is simply unit. In other words,
\\[ \\frac{da}{da} = 1.\\]

Next, let's consider the derivative of a type with respect to a different type. In this case, there
are no possible holes for the latter in the former. Therefore, the zipper is the empty datatype (the
datatype with no constructors).
\\[ \\frac{db}{da} = 0, \\text{ where } a \\neq b .\\]

This covers our base cases. Any type can be created by composing these simple types into larger
types using type addition and multiplication. Therefore, the only rules we need to consider are
the sum rule and the product rule. As a reminder, in regular differentiation, these are
\\[ \\begin{align}
\\frac{d}{dx}(f + g) &=& \\frac{df}{dx} + \\frac{dg}{dx} \\
\\frac{d}{dx} fg &=& f\\frac{dg}{dx} + g \\frac{df}{dx}
\\end{align} \\]

Let's consider the derivative of a sum type $x + y$ with respect to $a.$ We're looking for a
datatype that can represent all the holes where we could put a value of type $a.$ In this case, $a$
can either go somewhere in $x$ (whose zipper is $\frac{dx}{da}$) or somewhere in $y$ (whose zipper
is $\frac{dy}{da}$). Thus, the zipper of the sum is the sum type of the zippers. Voila!
Differentiation rule 1 is down!

Finally, for a product $xy,$ $a$ can either go somewhere in $x,$ in which case, we'll need to store
the value of $y$ (for context), or it can go somewhere in $y$, in which case we'll need to store
$x.$ Thus, the derivative of a product will be a sum $X + Y,$ where $X$ represents the constructors
that signal that the hole is somewhere in $X$ and $Y$ represents those where the hole is somewhere
in $y.$

Let's figure our $X$ and $Y.$

If $a$ goes in $x,$ then we'll need the zipper of $x$ to be able to represent the hole in $x.$
Packaged along with the context $y,$ this means that we need $X = y\frac{dx}{da}$ so that we can
represent any possible hole for $a$ in $x.$

Conversely, if $a$ goes in $y,$ then $Y = x \frac{dy}{da}.$
Therefore,
\\[\\frac{d(x + y)}{da} = x\\frac{dy}{da} + y\\frac{dx}{da}.\\]

## Conclusion

In this post, we've shown that the derivative of the algebraic representation of a datatype is
simply the algebraic derivative of that datatype. Additionally, we've shown an example where we
differentiated the rose tree type to arrive at a rose tree zipper, which we simplified into a form
that we could understand. Finally, we developed an intuition for why the rules work the way they do,
although we leave the rigorous proofs to more rigorous blogs (and journals!).

In the next post, I'll explore how we can generalize this feature so that we can generate zippers of
arbitrary types. Additionally, I'll examine how we can create zippers with multiple "holes" all of
which can interact so that we can simulate the notion of multiple pointers to the same data structure
as in an imperative language. Did I mention all of this will be in approximately $O(1)$ time? Stay
tuned!
