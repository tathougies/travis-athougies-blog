---
title: Beam
author: Travis Athougies
slug: beam
summary: "Beam is a Haskell interface to relational databases.  Beam is written in a pure Haskell, and doesn't make use of Template Haskell or non-intuitive embedded domain specific languages"
language: Haskell
status: PRODUCTION
priority: 0
github: "https://github.com/tathougies/beam"
version: 0.7.2.0
published: true
---

Beam is a Haskell interface to relational databases. Its key strengths are its type-safety and
intuitive syntax. Beam is written in pure Haskell, and doesn't make use of Template Haskell or
non-intuitive embedded domain specific languages.

## Documentation and Tutorials

Beam has comprehensive documentation on [its website](http://tathougies.github.io/beam/).

A series of tutorial for the current version of Beam can be found [there as
well](http://tathougies.github.io/beam/tutorials/tutorial1/).

You may also be interested in the [API reference on
hackage](http://hackage.haskell.org/package/beam-core) and in the [GitHub
sources](https://github.com/tathougies/beam).

## Use

Beam is ready to be used in production. The Beam API is fairly stable and
breaking changes follow the [Haskell PVP](https://pvp.haskell.org/).

Beam is available on both
[hackage](https://hackage.haskell.org/package/beam-core) and
[stackage](https://www.stackage.org/package/beam-core).

Beam has several backends:

* `beam-sqlite` -- Beam driver for the SQLite embedded database. [On
  hackage](https://hackage.haskell.org/package/beam-sqlite) and
  [stackage](https://www.stackage.org/package/beam-sqlite).
* `beam-postgres` -- Beam driver for Postgres. [On
  hackage](https://hackage.haskell.org/package/beam-postgres) and
  [stackage](https://www.stackage.org/package/beam-postgres).
* `beam-mysql` -- Beam driver for MySQL. Currently unreleased pending further
  tests, but available on [github](https://github.com/tathougies/beam-mysql).

There are some independent backends as well:

* `beam-firebird` -- Beam driver for FireBird. Available on [github](https://github.com/gibranrosa/beam-firebird).

Beam has a somewhat active IRC channel on freenode at `#haskell-beam` and a
google discussions group `beam-discussion`.

## Comparisons to other libraries

### `persistent`

- **Beam does not use Template Haskell.** Persistent requires the use of
  template haskell or very large manual instance declarations to create a
  database schema. The Template Haskell schema specification uses a non-Haskell
  DSL making projects using persistent hard to understand to those not familiar
  with persistent.
- **Beam supports basic SQL features like `JOIN`s or aggregates.** Persistent
  supports simple queries, but it does not support database-native joins or
  aggregations. Instead users are forced to use third-party libraries like
  esqueleto.
- **Beam also supports advanced features** Beam has full support for window
  functions and compound data types, like arrays. The postgres backend supports
  many postgres-specific types like `json` and `jsonb`.

### `opaleye`

- **Beam has simpler data types.** Opaleye requires that each table type is
  parameterized over each column in the table. This means that if your table
  contains 16 columns, it needs to have 16 type-level parameters. As you can
  imagine, this leads to incredibly long types. On the other hand, beam types
  are parameterized over only one type.
- **Beam does not need Template Haskell to derive instances and simplify
  types.** Hiding type definitions behind Template Haskell makes it difficult to
  understand what is going on with the code behind the scenes. Beam uses
  generics and regular Haskell types which are easy to reason about.
- **Beam generates readable SQL.** Opaleye's SQL can be very difficult to
  understand (see the example output on
  [GitHub](https://github.com/tomjaguarpaw/haskell-opaleye/tree/master/Doc/Tutorial)). Beam's
  SQL is easy to read and reason about. This makes it easier to debug.

### `HaskellDB`

- **Again, Beam does not use Template Haskell.** The Haskell type system is at
  an advanced enough state that we do not need to resort to Template Haskell
  EDSLs to do what most languages can in a straightforward manner. HaskellDB
  requires that types be created using the HaskellDB template haskell quoters.
- *To be continued...*
