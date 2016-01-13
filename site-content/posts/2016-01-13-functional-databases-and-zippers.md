---
title: Functional databases and zippers
author: Travis Athougies
tags: haskell, databases
published: false
---

Zippers are a transformation of an arbitrary algebraic data structure that enable O(1) updates to deeply nested structures even in purely functional languages.

In his articles on the subject, Oleg Kiselyov [lays the ground work](http://okmij.org/ftp/Haskell/Zipper2.lhs) for using zippers to enable concurrent, transactionalized access to a persistent, algebraic data structure.

As detailed in [my article](post:2015-01-16-zippers-derivatives-oh-what-fun), I go over the well-known fact that any algebraic data type has a corresponding zipper and that the type of the zipper is easily calculated by 'differentiating' the algebraic representation of the subject type.

In this article, I describe a database system (available on GitHub), that combines these two ideas to provide a persistent, ACID, algebraic data structure server. The database system is called Zippy.

## Motivations

In recent years, NoSQL database systems have grown in popularity. One reason developers choose NoSQL databases is Developers choose NoSQL databases is because the data they're storing does not fit neatly into the table paradigm. In response to this, databases such as MongoDB, Redis, and Neo4j offer either a variety of data structures or an alternative to tables.

Redis, for example, offers lists, maps, sets, and counters, among others. It is known for its speed and ease of use. However, writes are not durable, and data can be lost if the server is shut down disgracefully. Additionally, Redis stores all its data in memory, so the size of the database is limited to the system's memory. Finally, although easy to use, Redis does not have a complex query language, and commands do not run concurrently.

MongoDB offers the ability to store arbitrary JSON in its key-value database. However, it is limited in its querying ability to simple indices. Additionally, extending MongoDB with new data structures cannot be done in its native query language (javascript). Instead, developers would have to modify the C source directly.

Ultimately, most NoSQL databases suffer from the same pitfalls as most SQL databases. Namely, although they do not force developers into the table paradigm, they still force developers into doing things their way, either by limiting their query language or limiting their extensibility.

Fortunately, this does not have to be the case. Using the concept of zippers, zippy enables truly concurrent, durable, ACID access to an arbitrary algebraic data structures. Algebraic data structures are a well-studied group of data structures, and we can store all common data types in an algebraic structure.

Zippy offers a type-checked language known as Zephyr, a low-level, turing-complete, stack-based language, that can be used to beth define new data types and define transformations over data of these types. In fact, all of Zippy's data types (except for the primitive ints, strings, floats, and binary blobs) are defined in Zephyr itself.

In sum, Zippy offers fully durable, ACID access to arbitrary algebraic data and  a powerful query and manipulation language.

## Getting Started

Before we get started using zippy, you'll have to [download and install from GitHub](https://github.com/tathougies/zippy). Clone the repository and run `cabal install`. You may want to use a sandbox to avoid polluting your global cabal files.

In order to manipulate data in Zippy, you first need to define the types used in your database. Let's start by defining a data type to store information on a product. Open up a file `database1.zephyr` and type the following in.

```zephyr
PACKAGE database1

DATA Product ==
	CONS PhysicalProduct sku=Text title=Text description=Text price=Float weight=Float
    CONS DownloadableProduct sku=Text title=Text description=Text price=Float url=Text
```

Save the file, and run

```bash
zippy-serve -r database1.roots -d database1.data -Z ./database1.zephyr -T Product 9123
```

This tells zippy to start serving the database in files `database1.roots` and `database1.data` and type `Product` on port 9123. The `-Z` flag loads the file we just created.

Now run

```bash
telnet localhost 9123
```

This should open a prompt where you can type zippy commands. Type `cur` and hit enter. You should receive a message

```
cur
CurHasTag 0
```

Remember how we defined `Product` above? We said that it had two constructors. The servers response indicates that the database currently contains a product created with the first constructor, `PhysicalProduct`. This means that the database has the fields associated with that constructor.

When we query zippy, it offers us a limited view of our data structure. In order to examine the fields of the `PhysicalProduct` we're seeing, we'll need to use manipulation and navigation commands to dig deeper. Zippy offers this limited view in order to fully understand exactly which data our transaction needs to view. For example, if our transaction was going to set the name of the product to "gizmo" regardless of what was there, then we do not need to worry if another transaction changes the name to "gadget" right before, because the write-only transaction should always succeed. On the other hand, if our transaction read the name "gadget" and then wrote the name "gadget gizmo combo", then another transaction changing the name to "gizmo" would invalidate our transaction.

Let's explore the `PhysicalProduct` that zippy put there automatically. By default, when creating a new database, zippy defaults to the first available constructors of the root type and all its fields.

Type the `down 0` into the telnet session. This tells zippy to go into the first field of the `PhysicalProduct`. This is the `sku` field and it has type `Text`. You should receive a `Moved` back from zippy to indicate that the move succeeded. Now type `cur` again, you should receive back

```
cur
CurIsAtom TextD ""
```

This indicates that the current *focus* of the transaction is a primitive type (a `Text` in this case) with value "", the empty string. Let's go ahead and replace the sku with the string "gadget". Type `replace "gadget"` into the session. You should see the familiar `Moved` message. Now type `cur` again. You should now see

```
cur
CurIsAtom TextD "gadget"
```

Type `commit` to save the changes to the database. You should get back a `TxCommitted` message indicating that the changes made have been writted to disk and will now be visible to other transactions. Congratulations, you've successfully manipulated your first data in Zippy!

## Recursive types and Zephyr

You're probably underwhelmed at this point, because the above database had the root type of `Product`. This means that it can only store either one `PhysicalProduct` or one `DownloadableProduct`. This isn't what a database is meant for! Also, `move`, `cur`, `down`, and `up` are great, but this is hardly the great query language we were promised!

Enter recursive types and Zephyr. Usually, you want a database to store a collection -- ideally one that can be indexed for quick recall. In this section, we'll see how recursive algebraic types allow us to define an algebraic type that can grow to an arbitrary size. We'll also start using zippy's query language, zephyr, to give us elegant, type-checked access to the database. This will free us from ever having to use direct manipulation commands again.

Zephyr is a [stack-oriented programming language](https://en.wikipedia.org/wiki/Stack-oriented_programming_language), meaning that each program is a series of commands that manipulate a global stack. For example, the zephyr program

```zephyr
1
```

simply puhses the integer 1 to the stack. The program

```zephyr
1 2 +
```

pushes the integer 1 to the stack, followed by the integer 2, and then runs the `+` function, which replaces the top two entries with their sum. After this program runs, the stack will contain just the integer 3.

Zephyr supports other types, like strings. The program

```zephyr
"Hello, world" LOG
```

Will first push the string "Hello, world" onto the stack, and then run the `LOG` command, which will pop the top of the stack and print it. At the end of this program, the stack will be empty.

Zephyr is *type-checked*, so the program

```zephyr
1 "Hello, world" +
```

will cause the zephyr run-time to complain of a type error and refuse to run the program. This is convenient, because it offers us a way to verify our programs will never run a navigation command that will cause the database to become inconsistent.

Now that we understand zephyr, let's talk about how it will help us store a collection of products. As we said above, all of zippy's standard types are implemented in zephyr itself. The type that we're going to talk about now is called a *treap*. A [treap](http://en.wikipedia.org/wiki/Treap) is a randomized binary search tree that uses randomness to maintain balance. The randmoness ensures that it has good concurrency properties that minimize the amount of aborted transactions. Zephyr's treap implementation is located [here for reference](https://github.com/tathougies/zippy/blob/src/zephyr/treaps.zephyr).

