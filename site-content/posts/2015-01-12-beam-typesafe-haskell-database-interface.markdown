---
title: "Beam - A typesafe Haskell database interface"
author: Travis Athougies
tags: "haskell, database, types"
published: true
---

I just uploaded a new package to github. It's called *Beam* and it's a type-safe database interface
for Haskell.

Type safety and elegance are two of the main selling points of Haskell. However,
current Haskell database interface libraries (like Persistent and HaskellDB) are anything but. For instance, both
make extended use of Template Haskell. Although a useful language feature, Template Haskell suffers
from several disadvantages, namely its complexity and lack of type safety. Additionally, both fail to completely cover all common SQL use cases. For example, Persistent doesn't even support foreign keys. 

It doesn't have to be this way. Several new GHC extensions, such as Generics, Closed Type Families, and Default Signatures allow us to elegantly and succinctly express everything that these libraries formerly used Template Haskell for. The best part is that we can do this without introducing metaprogramming or unintuitive DSLs. Instead, we rely on simple, intuitive Haskell syntax.

## Defining our first Beam database schema

In Beam, everything is done with plain old Haskell data types. Let's define a simple todo list database in beam, and then use this schema to make queries on a SQLite3 database. We begin by defining type-level names for our columns. We will have two tables in our schema: one for todo lists and one for todo items. A todo list will have two columns: a name and a description. A todo item will have three: a name, a description, and a due date. We will need to use the `DeriveGeneric` and `DeriveDataTypeable` extensions in order to allow our names to play nice with Beam.

```haskell
{-# LANGUAGE DeriveGeneric, DeriveDataTypeable #-}
import GHC.Generics
import Data.Typeable
data Name = Name deriving (Generic, Typeable)
data Description = Description deriving (Generic, Typeable)
data DueDate = DueDate deriving (Generic, Typeable)
```

Using these names we can define our Haskell-level data types for our tables. Notice that these definition are given in plain old Haskell. There are no fancy template Haskell DSLs here!

```haskell
import Database.Beam
data TodoListRef = TodoListRef deriving (Generic, Typeable)
data TodoList = TodoList
              { todoListName :: Column Name Text
              , todoListDescription :: Column Description Text }
                deriving (Show, Generic, Typeable)
data TodoItem = TodoItem
              { todoItemList :: ForeignKey TodoList TodoListRef
              , todoItemName :: Column Name Text
              , todoItemDescription :: Column Description Text }
                deriving (Show, Generic, Typeable)
```

All we have left is to define a few instances, so our type is Beam compatible. These instances will be auto-generated for us by sensible Beam defaults, but they also give us a lot of flexibility later on if we want to customize exactly how each field is mapped to lower-level SQL. The rules for which instances we need to instantiate are easy. They are:

1. For each Haskell data type that we want to map to a SQL table, we need to instantiate the `Table` type class.
2. For each name for a `Column` in the table, we need to instantiate a `Field` instance referencing both the table and the name.
3. For each name for a `ForeignKey` in the table, we need to instantiate a `Reference` instance refererncing both the table and the name.

Applying these rules is straigtforward. First, we apply rule 1 to generate two instances:

```haskell
instance Table TodoList
instance Table TodoItem
```

Next, we instantiate a `Field` instance for each column, and a `Reference` instance for each `ForeignKey`.

```haskell
instance Field TodoList Name
instance Field TodoList Description
instance Field TodoItem Name
instance Field TodoItem Description
instance Reference TodoItem TodoListRef
```

Voilà! That's it! These type are ready to be used in Beam.

## Querying the database

We're almost ready to use these types in a real database. First though, we need to create a database with the right tables. This is easy to do in Beam. We simply create a `Database` object.

```haskell
todoListDb :: Database
todoListDb = database_
           [ table_ (schema_ :: TodoList)
           , table_ (schema_ :: TodoItem) ]
```

Now, we can use this object to allow Beam to automatically migrate a database to match this database schema. Let's open a SQLite3 database in Beam!

```haskell
> import Database.Beam.Backend.Sqlite3
> beam <- openDatabase todoListDb (Sqlite3Settings "beam.db")
```

### Creating some test data

Before we can query this, let's add in some test data. We'll add in two lists with two items each, and one empty todo list.

```haskell
> :set -XOverloadedStrings
> :{
| let todoLists = [ TodoList (column "List 1") (column "Description for list 1")
|                 , TodoList (column "List 2") (column "Description for list 2")
|                 , TodoList (column "List 3") (column "Description for list 3") ]
| :}
> [list1, list2, list3] <- inBeamTxn beam $ mapM insert todoLists
> :{
| let todoItems = [ TodoItem (ref list1) (column "Item 1") (column "This is item 1 in list 1")
|                 , TodoItem (ref list1) (column "Item 2") (column "This is item 2 in list 1")
|                 , TodoItem (ref list2) (column "Item 1") (column "This is item 1 in list 2")
|                 , TodoItem (ref list2) (column "Item 2") (column "This is item 2 in list 2") ]
| :}
> inBeamTxn beam $ mapM insert todoItems
[QueryTable (Column 1) (ForeignKey (Column 1) :|: Column "Item 1" :|: ...]
```

### Our first query

First, let's try to get all the todo lists.

```haskell
> inBeamTxn beam $ queryList (all_ (of_ :: TodoList))
```

We're using the `queryList` function to get our results as a list. The normal `query` function returns a `Source` from the `conduit` package, which is usually easier to work with when writing an application, but is not as intuitive when working on the command line.

Now, let's try to get all the `TodoItem`s associated with `list1`.
  
```haskell
> inBeamTxn beam $ queryList (all_ (of_ :: TodoItem) `where_` (\todoItem -> todoItem # TodoListRef ==# pk_ list1)
```

Next, let's write a query to get the `TodoItem`s along with their `TodoList`s.

```haskell
> inBeamTxn beam $ queryList (all_ (of_ :: TodoItem) @->* TodoListRef)
```

This query will map to a SQL inner join. Suppose you wanted all `TodoList`s regardless of whether they had an associated `TodoItem`. In this case, we can simply use the right join selection

```haskell
> inBeamTxn beam $ queryList (all_ (of_ :: TodoItem) @?->* TodoListRef)
```