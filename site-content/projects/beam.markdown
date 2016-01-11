---
title: Beam
author: Travis Athougies
slug: beam
summary: Beam is a Haskell interface to relational databases.  Beam is written in a pure Haskell, and doesn't make use of Template Haskell or non-intuitive embedded domain specific languages
language: Haskell
status: ALPHA
priority: 0
github: https://github.com/tathougies/beam
version: 2.0.0
---

Beam is a Haskell interface to relational databases. Its key strengths are its type-safety and
intuitive syntax. Beam is written in a pure Haskell, and doesn't make use of Template Haskell or
non-intuitive embedded domain specific languages. Instead it makes extensive use of Generics, Closed
Type Families, and other elements of Haskell's strong type system. Unlike other popular Haskell
database interfaces, Beam tries to work with the compiler rather than in spite of it.

## Tutorial

A tutorial for the current version of beam can be found in [this post](post:2015-01-12-beam-typesafe-haskell-database-interface)

## Known limitations

Beam is still in a very experimentl stage. You should not use Beam for production systems yet. While
most SQL works correctly when dealing with tables, there are still known problems in dealing with
projections involving standalone expressions. These will be fixed in time.

Additionally, the Beam API is still in a constant state of flux. In some ways, it's a bit obtuse,
since I basically designed it to match the way I think. However, this is probably unintuitive for
many people. As more people use the library and more feedback is generated, the API will be adapted
to make it more intuitive and understandable.

Lastly, Beam currently outputs all the SQL that it runs onto stdout, which is not very elegant, but
helps me with debugging!

That being said, please file issues and comments on GitHub! I'm always looking for feedback, and
pull requests are always welcome!
