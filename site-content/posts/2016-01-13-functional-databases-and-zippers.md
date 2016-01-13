---
title: Functional databases and zippers
author: Travis Athougies
tags: haskell, databases
published: false
---

Zippers are a transformation of an arbitrary algebraic data structure that enable O(1) updates to deeply nested structures even in purely functional languages.

In his articles on the subject, Oleg Kiselyov [lays the ground work](http://okmij.org/ftp/Haskell/Zipper2.lhs) for using zippers to enable concurrent, transactionalized access to a persistent, algebraic data structure.

As detailed in [my article](post:zippers-derivatives-oh-what-fun), I go over the well-known fact that any algebraic data type has a corresponding zipper and that the type of the zipper is easily calculated by 'differentiating' the algebraic representation of the subject type.

In this article, I describe a database system (available on GitHub), that combines these two ideas to provide a persistent, ACID, algebraic data structure server. The database system is called Zippy.

## Motivations

In recent years, NoSQL database systems have grown in popularity. One reason developers choose NoSQL databases is Developers choose NoSQL databases is because the data they're storing does not fit neatly into the table paradigm. In response to this, databases such as MongoDB, Redis, and Neo4j offer either a variety of data structures or an alternative to tables.

Redis, for example, offers lists, maps, sets, and counters, among others. It is known for its speed and ease of use. However, writes are not durable, and data can be lost if the server is shut down disgracefully. Additionally, Redis stores all its data in memory, so the size of the database is limited to the system's memory. Finally, although easy to use, Redis does not have a complex query language, and commands do not run concurrently.

MongoDB offers the ability to store arbitrary JSON in its key-value database. However, it is limited in its querying ability to simple indices. Additionally, extending MongoDB with new data structures cannot be done in its native query language (javascript). Instead, developers would have to modify the C source directly.

Ultimately, most NoSQL databases suffer from the same pitfalls as most SQL databases. Namely, although they do not force developers into the table paradigm, they still force developers into doing things their way, either by limiting their query language or limiting their extensibility.

Fortunately, this does not have to be the case. Using the concept of zippers, zippy enables truly concurrent, durable, ACID access to an arbitrary algebraic data structures. Algebraic data structures are a well-studied group of data structures, and we can store all common data types in an algebraic structure.

Zippy offers a type-checked language known as Zephyr, a low-level, turing-complete, stack-based language, that can be used to beth define new data types and define transformations over data of these types. In fact, all of Zippy's data types (except for the primitive ints, strings, floats, and binary blobs) are defined in Zephyr itself.

In sum, Zippy offers fully durable, ACID access to arbitrary algebraic data and  a powerful query and manipulation language.

