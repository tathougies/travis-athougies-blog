---
title: "Beam - A typesafe Haskell database interface"
author: Travis Athougies
tags: "haskell, web"
published: true
---

I just uploaded a new package to github. It's called *Beam* and it's a type-safe database interface
for Haskell.

Type safety and expressive power are two of the main selling points of Haskell. However, current
Haskell database interface libraries (like Persistent and HaskellDB) are anything but. For instance,
both make extended use of Template Haskell. Although a useful language feature, Template Haskell
suffers from several disadvantages, namely its complexity and lack of type safety. Additionally,
both fail to cover several common SQL use cases. For example, Persistent doesn't even support
foreign keys and joins.

It doesn't have to be this way. Haskell is increasingly being used on the web, and in order to be a
serious web language, Haskell needs a good database backend. Several new GHC extensions, such as
Generics, Closed Type Families, and Default Signatures allow us to elegantly and succinctly express
everything that Yesod and HaskellDB used Template Haskell for. In terms of power, nothing stops
these libraries from fully realizing the power of SQL, but it does help to start from clean and
simple abstractions.

To summarize, Beam's primary features and differentiators are:

- **Declarative syntax** - Beam schemas are defined declaratively at the type-level as POHT (plain
    old Haskell types). Type-level programming and generic deriving is used to automatically derive
    instances and provide proper type-checking.
- **Comprehensive** - Beam aims to be a comprehensive library. This means that it will at least
    support all standard SQL features, including all types of joins and nested queries. This is
    incomplete as of now.
- **Leaky** - Beam does not attempt to shield the user from the idiosyncracies of the underlying
    RDBMS. Users should use the RDBMS that they are most familiar with and that best meets the
    performance and feature requirements of the application. This leakiness also means that Beam can
    expose backed-specific functions to user queries. For example, PostgreSQL backend could easily
    provide PostgreSQL-specific functions to work with arrays, JSON, and GIS data. Because of
    Haskell, these functions could be type-checked statically at compile time.
- **Backend-agnostic** - Although Beam does not try to standardize RDBMS behavior, the core does aim
    to be agnostic of the underlying backend. If you use only standard Beam features in your queries
    and standard data types in your schemas, then you should be able to swap the backend out anytime
    (including at run time) and have the same SQL generated (notice I said the *same SQL* -- this
    does not mean that the results will be the same).
- **No Template Haskell** - Beam does not make use of Template Haskell. Rather, it uses several
    advanced extensions, such as `DeriveGeneric`, `DeriveDataTypeable`, `TypeFamilies`,
    `MultiParamTypeClasses`, etc. Although this means that Beam is not 100% Haskell 2010, these
    syntax extensions embody the "spirit of Haskell" much better than Template
    Haskell. Additionally, unlike Template Haskell, these extensions are not so much tied to the
    structure of GHC.

## Defining our first Beam database schema

In Beam, everything is done via plain old Haskell data types. Let's define a simple todo list
database in Beam, and then use this schema to make queries on a SQLite3 database. We begin by
defining type-level names for our columns. We will have two tables in our schema: one for todo lists
and one for todo items. A todo list will have two columns: a name and a description. A todo item
will have three: a name, a description, and a foreign key to the list it belongs to. We will need to
use the `DeriveGeneric` and `DeriveDataTypeable` extensions in order to allow our data types to play
nicely with Beam.

Notice that these definition are given in plain old Haskell. No crufty Template Haskell DSLs here!

```haskell
{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, StandaloneDeriving, OverloadedStrings, FlexibleInstances #-}
module BeamExample where

import Database.Beam
import Data.Text (Text)
data TodoList column = TodoList
                    { todoListName        :: column Text
                    , todoListDescription :: column Text }
                deriving (Generic, Typeable)
data TodoItem column = TodoItem
                     { todoItemList        :: ForeignKey TodoList column
                     , todoItemName        :: column Text
                     , todoItemDescription :: column Text }
                       deriving (Generic, Typeable)
```

### Column constructors

Notice that each of our types takes in a special `column` type argument. This is called a *column
constructor* and is used by beam to co-opt our data type to play several different roles. Beam will
use these data types in several different contexts, such as to set column options, to construct
query clauses, and to store data.

Most of the time, we'll be using the `Column` column constructor, which is defined as a simple
newtype.
```haskell
newtype Column = Column a
```
Because it's a newtype, using `Column` has no runtime overhead, but it does mean that we will need
to explicitly wrap and unwrap `Column` values. This can be done with the `column` and `columnValue`
functions, which have the types.
```haskell
column :: a -> Column a
columnValue :: Column a -> a
```

The other common column constructor is `Nullable Column`. This wraps the stored value with `Maybe`
and lets you make nullable foreign keys. The `column` and `columnValue` functions are overloaded to
work on this type as well.

```haskell
column :: Maybe a -> Nullable Column a
columnValue :: Nullable Column a -> Maybe a
```

### A note on deriving

Because of the complicated nature of our types, GHC won't be able to derive `Show` instances using
the regular `deriving` mechanism, but this is easily fixed with `StandaloneDeriving.` We only need
to define instances of `Show` for our types parameterized with the special `Column`
constructor.

```haskell
deriving instance Show (TodoList Column)
deriving instance Show (TodoItem Column)
```

## Interfacing with Beam

Now that we've defined our table data types, we need to define a few instances so that Beam can work
its magic. For each of our table types, we'll need to instantiate the `Table` type class.
```haskell
instance Table TodoItem
instance Table TodoList
```
When GHC 7.10 comes out, we will be able to specify these `Table` instances as part of the
`deriving` declaration with the `DeriveAnyClass` extension, but for now they have to be separate.

Because of the `Generic` instances, Beam can fully derive this class for us. The `Table` instance
for a type controls how it's mapped to SQL. Among other things, it determines its table name, the
column names, and the column types and constraints. The default instance names the table after the
Haskell type, and names the columns after the selector names. It also chooses an appropriate SQL
type to hold the Haskell datatype. Finally, it adds a primary key column named "id." You can override
these instances if you'd like to rename a column, rename the table, or set the SQL type of a column,
but most of the time, the default one is fine.

Voilà! That's it! These type are ready to be used in Beam.

## Querying the database

We're almost ready to use these types in a real database. First though, we need to create a
`Database` object with the right tables.
```haskell
todoListDb :: Database
todoListDb = database_
           [ table_ (schema_ :: Simple TodoList)
           , table_ (schema_ :: Simple TodoItem) ]
```
`Simple table` is a type synonym for `table Column,` so `Simple TodoList` is simply our `TodoList`
datatype with the standard `Column` constructor.

Now, we can use this object to allow Beam to automatically migrate a database to match this database
schema. Save all your work in a Haskell file, fire up GHCi, and let's begin.

```haskell
> :load <name-of-file>
> import Database.Beam.Backend.Sqlite3
> beam <- openDatabase todoListDb (Sqlite3Settings "beam.db")
```

### Creating some test data

Before we can query this, let's add in some test data. We'll add in two lists with two items each,
and one empty todo list.

```haskell
> :set -XOverloadedStrings
> :{
| let todoLists = [ TodoList (column "List 1") (column "Description for list 1")
|                 , TodoList (column "List 2") (column "Description for list 2")
|                 , TodoList (column "List 3") (column "Description for list 3") ]
| :}
> Success [list1, list2, list3] <- inBeamTxn beam $ mapM insert todoLists
...
> :{
| let todoItems = [ TodoItem (ref list1) (column "Item 1") (column "This is item 1 in list 1")
|                 , TodoItem (ref list1) (column "Item 2") (column "This is item 2 in list 1")
|                 , TodoItem (ref list2) (column "Item 1") (column "This is item 1 in list 2")
|                 , TodoItem (ref list2) (column "Item 2") (column "This is item 2 in list 2") ]
| :}
> inBeamTxn beam $ mapM insert todoItems
...
```

## Our first query

First, let's try to get all the todo lists.

```haskell
> inBeamTxn beam $ queryList (all_ (of_ :: Simple TodoList))
[Entity (PK (Column 1) (TodoList (Column "List 1") (Column "Description for list 1")), ...]
```

We're using the `queryList` function to get our results as a list. The normal `query` function
returns a `Source` from the `conduit` package, which is usually easier and safer to work with when
writing an application, but is not as intuitive when working on the command line.

To understand queries, let's take a look at the types.
```haskell
> :type (all_ (of_ :: Simple TodoList))
all_ (of_ :: Simple TodoList) :: Query (Entity TodoList Column)
```
All queries have types `Query a`. When run, queries of type `Query a` return rows of type `a`. The
`all_` query returns a `Query` that returns rows from a given table. The `Entity` type packages a
table (the data types we defined) along with its *phantom fields* (defined in the type
class). *Phantom fields* are fields that exist in the database but are not mapped to our data
type. By default, we use the phantom fields to store the table's primary key. Therefore, an
`Entity a Column`, by default, stores the table `a` and its primary key.

### Relationships

Now, let's try to get all the `TodoItem`s associated with `list1`.
```haskell
> inBeamTxn beam $ queryList (todoItemList <-@ list1)
```
The `f <-@ query` combinator takes in an entity(`query`) and a selector(`f`) from another table that
is a `ForeignKey` to that entity, and returns all elements of table where the `ForeignKey` points to
the entity.

Next, let's write a query to get the `TodoItem`s along with their `TodoList`s.

```haskell
> inBeamTxn beam $ queryList (all_ (of_ :: Simple TodoItem) ==> todoItemList)
```
The `q ==> f` combinator takes a query (`q`) and a selector for the table that references a
`ForeignKey`.  It returns a query that performs an inner join.

Suppose you wanted all `TodoList`s regardless of whether they had an associated `TodoItem`. In this
case, we can simply use the right join selection

```haskell
> inBeamTxn beam $ queryList (all_ (of_ :: Simple TodoList) <=? todoItemList)
```

For more complicated queries, see the
[example on github](https://github.com/tathougies/beam/blob/master/example/TodoList.hs).

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

That being said, please leave your comments! I'm always looking for feedback.
