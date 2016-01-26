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
- [Beam Tutorial (part 3)](post:2016-01-25-beam-tutorial-part-3)

You may also be interested in the [API reference on hackage](http://hackage.haskell.org/package/beam) and in the [GitHub sources](https://github.com/tathougies/beam).

## Known limitations

Beam is ready to be used in production, but you may encounter some bugs. You should check the SQL that is being generated to verify that the correct queries are being issued. You can have beam dump the queries to the console by using `openDatabaseDebug` to open your beam database.

The beam API is fairly stable, but there may be some minor changes. In each case, I'll document how to update your code in case of any breaking changes.

Please send any bug reports or pull requests to [GitHub](https://github.com/tathougies/beam). If you have any beam-specific questions, feel free to e-mail me at travis at athougies.net.

## Comparisons to other libraries

### `persistent`

- **Beam does not use Template Haskell.** Persistent requires the use of template haskell or very large manual instance declarations to create a database schema. The Template Haskell schema specification uses a non-Haskell DSL making projects using persistent hard to understand to those not familiar with persistent.
- **Beam is SQL-only, but supports advanced SQL features like `JOIN`s or aggregates.** Persistent supports simple queries, but it does not support database-native joins or aggregations. Instead users are forced to use third-party libraries like esqueleto.

### `opaleye`

- **Beam has simpler data types.** Opaleye requires that each table type is parameterized over each column in the table. This means that if your table contains 16 columns, it needs to have 16 type-level parameters. As you can imagine, this leads to incredibly long types. On the other hand, beam types are parameterized over only one type.
- **Beam does not need Template Haskell to derive instances and simplify types.** Hiding type definitions behind Template Haskell makes it difficult to understand what is going on with the code behind the scenes. Beam uses generics and regular Haskell types which are easy to reason about.
- **Beam generates readable SQL.** Opaleye's SQL can be very difficult to understand (see the example output on [GitHub](https://github.com/tomjaguarpaw/haskell-opaleye/tree/master/Doc/Tutorial)). Beam's SQL is easy to read and reason about. This makes it easier to debug.

### `HaskellDB`

- **Again, Beam does not use Template Haskell.** The Haskell type system is at an advanced enough state that we do not need to resort to Template Haskell EDSLs to do what most languages can in a straightforward manner. HaskellDB requires that types be created using the HaskellDB template haskell quoters.
- *To be continued...*
