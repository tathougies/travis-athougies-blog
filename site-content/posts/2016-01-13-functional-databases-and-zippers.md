---
title: Functional databases and zippers
author: Travis Athougies
tags: haskell, databases
---

Zippers are a transformation of an arbitrary algebraic data structure that enable O(1) updates to deeply nested structures even in purely functional languages.

In his articles on the subject, Oleg Kiselyov [lays the ground work](http://okmij.org/ftp/Haskell/Zipper2.lhs) for using zippers to enable concurrent, transactionalized access to a persistent, algebraic data structure.

As detailed in [my article](post:zippers-derivatives-oh-what-fun), I go over the well-known fact that any algebraic data type has a corresponding zipper and that the type of the zipper is easily calculated by 'differentiating' the algebraic representation of the subject type.

In this article, I describe a database system (available on GitHub), that combines these two ideas to provide a persistent, ACID, algebraic data structure server. The database system is called Zippy.

## Motivations

In recent years, NoSQL database systems have grown in popularity. One reason developers choose NoSQL databases is Developers choose NoSQL databases is because the data they're storing does not fit neatly into the table paradigm. In response to this, databases such as MongoDB, Redis, and Neo4j offer either a variety of data structures or an alternative to tables.

Redis, for example, offers lists, maps, sets, and counters, among others. It is known for its speed and ease of use. However, writes are not durable, and data can be lost if the server is shut down disgracefully. Additionally, Redis stores all its data in memory, so the size of the database is limited to the system's memory.

MongoDB 

## A New Post

Enter text in [Markdown](http://daringfireball.net/projects/markdown/). Use the toolbar above, or click the **?** button for formatting help.
