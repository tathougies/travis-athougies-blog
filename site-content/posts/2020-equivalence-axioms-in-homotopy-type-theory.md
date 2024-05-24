---
title: "Equivalence axioms in homotopy type theory"
author: "Travis Athougies"
tags: "type theory,math,haskell,HoTT"
published: true
---

Homotopy type theory is a novel type theory in which ideas from
mathematical topology are combined with those from type theory and the
lambda calculus to arrive at what is hoped to be a better foundation
for mathematics.

In particular, homotopy type theory extends regular type theoretical
notions of what it means to be _equal_. Many newcomers to traditional
type theories, such as those used in Coq and Agda, are surprised to
learn that seemingly obvious facts can neither be proved nor used for
computation. For example, mathematicians and functional programmers
both depend on the idea of _function extensionality_, which states
that any two functions $f$ and $g$ are completely characterized by
their inputs and outputs. Thus, it would stand to reason in this line
of thinking that, if $f, g : A \rightarrow B$ and for all $x : A$,
$f(x) = g(x)$ then $f = g$.

However, this cannot be stated directly in Coq or Agda. Instead, if
such a thing is used in a proof, it must be introduced by an axiom.

As another example, mathematicians are used to identifying structures
that are equivalent. For example, we are used to calculating with the
rational numbers $\mathtt{Q}$. Every child knows how to add
$\frac{1}{2}$ and $\frac{3}{4}$ by turning $\frac{1}{2}$ into
$\frac{2}{4}$. Formally, the rational numbers can be defined (in set
theoretical terms) as the equivalence classes of the set $\{ (a, b) \|
a,b \in \mathtt{N}, b \neq 0 \}$ over the equivalence $a_{1}b_{2} =
a_{2}b_{1}$. However, informally, we write rational numbers as
fractions $\frac{x}{y}$ or decimal expansions $1.59$, which stands for
$\frac{159}{100}$ or $5.\overline{3}$, which stands for
$\frac{16}{3}$. If we wanted to formally identify how these map to our
formal definition, we can define functions $fractorational$ or
$dectorational$ that provide a bijection between the formal definition
and the informal one. Then, we can say that both representations are
equivalent.

Informally, the same "set" can be notated in multiple different ways,
as long as we can describe how the different notations are equivalent
in a systematic way.

In standard type theory, such a characterization of equivalent types
is again forced to be described as an axiom, rather than something
more fundamental. In homotopy type theory, we are also forced to
axiomatize this part of the type theory, since it cannot be directly
proven with any other parts of the theory. This is called 'univalence'
or the 'univalence axiom', and although it makes homotopy type theory
a great description of mathematics, it also makes it impossible to use
as a computational model, since we cannot compute over axioms.

Stepping back further, homotopy type theory defines equality between
two terms in a type $A$ as an inductive type $Id_A : A -> A -> U$
(where $U$ is the type of universes) with one constructor $refl_A :
\prod_{a : A} Id_A(a, a)$. In particular, this says that, in order for
two terms to be equal, they must be the same term. Homotopy type
theory now says that, for function extensionality, two functions that
are equivalent by function extensionality are not only 'equivalent'
but that they are in fact the exact same. On the other hand,
traditional type theory suggests that two functions with different
definitions, while they may act similarly, are not exactly the
same. In other words, in a language like Coq, Agda, or even Haskell,
given two functions.

<!-- TODO add f x = x + 0, g x = x -->

We cannot form a $refl$ term between them because they have different
definitions. But in homotopy type theory, because $f$ and $g$ agree at
all $x$, we can.

Computational interpretations of homotopical type theory provide means
of constructing terms of type $Id_{Nat \rightarrow Nat}$ that have
additional structure. For example, in cubical type theory, the path
object ... is a demonstration that $f$ and $g$ are equivalent, and
moreover, belongs to the $Id_{Nat \rightarrow Nat}$ type, which means
that -- $refl$ being the only generator of $id$ -- that it is
equivalent to $refl$.

But how do we square away the fact that this path is not exactly
$refl$ with the fundamental definition of $Id$. If $Id$ is generated
solely by $refl$, then why is this new, non-$refl$ path object the
same as $refl$? This post seeks to answer that question in a way that
satisfies both the computer scientists and homotopy theorists.
