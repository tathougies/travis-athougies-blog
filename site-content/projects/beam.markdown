---
title: Beam
author: Travis Athougies
slug: beam
summary: "Beam is a Haskell interface to relational databases.  Beam is written in a pure Haskell, and doesn't make use of Template Haskell or non-intuitive embedded domain specific languages"
language: Haskell
status: ALPHA
priority: 0
github: "https://github.com/tathougies/beam"
version: 2.0.0
published: true
---


Beam is a Haskell interface to relational databases. Its key strengths are its type-safety and
intuitive syntax. Beam is written in pure Haskell, and doesn't make use of Template Haskell or
non-intuitive embedded domain specific languages. Instead it makes extensive use of Generics, Closed
Type Families, and other elements of Haskell's strong type system. Unlike other popular Haskell
database interfaces, Beam tries to work with the compiler rather than in spite of it.

## Documentation and Tutorials

A series of tutorial for the current version of beam can be found in these tutorials:

- [Beam Tutorial (part 1)](post:2016-01-21-beam-tutorial-1)
- [Beam Tutorial (part 2)](post:2016-01-22-beam-tutorial-part-2)
- [Beam Tutorial (part 3)](post:2016-01-25-beam-tutorial-part-3

You may also be interested in the [API reference on hackage](http://hackage.haskell.org/package/beam) and in the [GitHub sources](https://github.com/tathougies/beam).

## Known limitations

Beam is ready to be used in production, but you may encounter some bugs. You should check the SQL that is being generated to verify that the correct queries are being issued. You can have beam dump the queries to the console by using `openDatabaseDebug` to open your beam database.

The beam API is fairly stable, but there may be some minor changes. In each case, I'll document how to update your code in case of any breaking changes.

Please send any bug reports or pull requests to [GitHub](https://github.com/tathougies/beam). If you have any beam-specific questions, feel free to e-mail me at travis at athougies.net.
