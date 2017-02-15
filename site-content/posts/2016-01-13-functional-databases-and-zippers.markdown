---
title: Functional databases and zippers
author: Travis Athougies
tags: "haskell, databases"
published: false
---

Zippers are a transformation of an arbitrary algebraic data structure that enable O(1) updates to deeply nested structures even in purely functional languages.

In his articles on the subject, Oleg Kiselyov [lays the ground work](http://okmij.org/ftp/Haskell/Zipper2.lhs) for using zippers to enable concurrent, transactionalized access to a persistent, algebraic data structure.

As detailed in [my article](post:2015-01-16-zippers-derivatives-oh-what-fun), I go over the well-known fact that any algebraic data type has a corresponding zipper and that the type of the zipper is easily calculated by 'differentiating' the algebraic representation of the subject type.

In this article, I describe a database system (available on GitHub), that combines these two ideas to provide a persistent, ACID, algebraic data structure server. The database system is called Zippy.

## Features

Below is a summary of Zippy's features:

1. **Immutable** -- All data in zippy is stored immutably. Once stored, data is never discarded. Updates are handled by creating a new version of the database. This performs well due to the pure nature of the Zephyr query language -- since all data is immutable, we are free to share large portions of the data structure, like Haskell
2. **Lock-free** -- Data in zippy is manipulated using zippers. Zippers allow us to focus on particular parts of an algebraic data structure and make changes. Zippy does not perform any locking for reads or writes. Instead, the zipper movements and changes are written to an in-memory log. When it comes time to commit, the log is verified and replayed, to ensure linearizability.
3. **Append-only** -- Similar to CouchDB, Zippy never seeks to write in its data file. New portions of the data structure are streamed out to the data file in order. Of course, reads may require seeks.
4. **Richly- and strongly-typed** -- Zippy is not schemaless! Every database must be given a type. Zippy supports polymorphic types and polymorphic queries. Additionally, it comes with built-in implementations of several persistent data structures designed for concurrency including random treaps, hash tries, finger trees, etc.

## Motivations

The main motivation behind Zippy was to apply Haskell to a real-world problem. I wanted to show that you can write extremely performant Haskell code without losing any elegance.

NoSQL databases have grown more popular in recent years, as we've discovered the limitations of the relational paradigm. Many NoSQL databases now include features that seem oh-too-familiar to the functional programming language community such as immutability (CouchDB), rich data types (MongoDB and Redis), and shared-nothing concurrency (Cassandra, CouchDB).

With this in mind, I set out to create a Haskell database that stores Haskell data. Algebraic data types are well-studied, and we know how to fit all types of data into their structure. Additionally, they are infinitely more flexible than tables and relations. The world just seemed ready for a Haskell database to take on the world!

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

simply pushes the integer 1 to the stack. The program

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

The definition of the treap type is copied here for reference:

```zephyr
DATA Treap k v ==
  CONS Leaf
  CONS Branch key=k value=v prio=Integer left=(Treap k v) right=(Treap k v)
```

This declaration says that a `Treap k v` is either a `Leaf` or a `Branch`. A `Leaf` has no fields, whereas a `Branch` has five fields, including two recursive ones (`left` and `right`). Those familiar with Haskell type declarations will immediately notice that `Treap` is a parameterized type, meaning that the types `k` and `v` are not known until the type is *instantiated*. For example, `Treap Int Text` is the type of treaps with keys of type `Int` and values of type `Text`.

Let's create a database for our products. Open up `database2.zephyr` and enter the following.

```zephyr
DATA ProductsDatabase ==
  CONS ProductsDatabase by-sku=(Treap Text Product)

DEFINE add-physical-product ==
  !(ProductsDatabase | *s Text Text Text Float Float)
  PhysicalProduct
  [ CUT ] VISIT-PhysicalProduct-sku
  SWAP
  [[text-compare] DIP] DIP
  [ insert-treap ] VISIT-by-sku
  
DEFINE add-downloadable-product ==
  !(ProductsDatabase | *s Text Text Text Float Text)
  DownloadableProduct
  [ CUT ] VISIT-DownloadableProduct-sku
  SWAP
  [[text-compare] DIP ] DIP
  [ insert-treap ] VISIT-by-sku
  
DEFINE find-product-by-sku ==
  !(ProductsDatabase | *s Text)
  
  { We cut the zipper at the root to create a copy of the zipper that will not
    be persisted on commit. This is useful to avoid logging reads that we never
    need to verify, since this query is read-only }
  CUT
  [ [ text-compare ] { comparator }
    [ CUT Just ] { on found, cut the branch here, and yield it }
    [ Nothing ] { on not found, return Nothing }
    [ treap-find ] VISIT-ProductsDatabase-by-sku
    YIELD ] ENTER-ZIPPER
```
