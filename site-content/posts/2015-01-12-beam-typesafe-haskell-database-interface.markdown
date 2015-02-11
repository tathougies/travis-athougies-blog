---
title: "Beam - A typesafe Haskell database interface"
author: Travis Athougies
tags: "haskell, web"
published: true
---

I just uploaded a new package to github. It's called *Beam* and it's a type-safe database interface
for Haskell.

Type safety and expressive power are two of the main selling points of Haskell. However,
current Haskell database interface libraries (like Persistent and HaskellDB) are anything but. For instance, both
make extended use of Template Haskell. Although a useful language feature, Template Haskell suffers
from several disadvantages, namely its complexity and lack of type safety. Additionally, both fail to cover several common SQL use cases. For example, Persistent doesn't even support foreign keys and joins. 

It doesn't have to be this way. Haskell is increasingly being used on the web, and in order to be a serious web language, Haskell needs a good database backend. Several new GHC extensions, such as Generics, Closed Type Families, and Default Signatures allow us to elegantly and succinctly express everything that Yesod and HaskellDB used Template Haskell for. In terms of power, nothing stops these libraries from fully realizing the power of SQL, but it does help to start from clean and simple abstractions.

To summarize, Beam's primary features and differentiators are:

- **Declarative syntax** - Beam schemas are defined declaratively at the type-level as POHT (plain old Haskell types). Type-level programming is used to automatically derive instances and provide proper type-checking.
- **Comprehensive** - Beam aims to be a comprehensive library. This means that it will at least support all standard SQL features, including all types of joins and nested queries. This is incomplete as of now.
- **Leaky** - Beam does not attempt to shield the user from the idiosyncracies of the underlying RDBMS. Users should use the RDBMS that they are most familiar with and that best meets the performance and feature requirements of the application. This leakiness also means that Beam can expose backed-specific functions to user queries. For example, PostgreSQL backend could easily provide PostgreSQL-specific functions to work with arrays, JSON, and GIS data. Because of Haskell, these functions could be type-checked statically at compile time.
- **Backend-agnostic** - Although Beam does not try to standardize RDBMS behavior, the core does aim to be agnostic of the underlying backend. If you use only standard Beam features in your queries and standard data types in your schemas, then you should be able to swap the backend out anytime (including at run time) and have the same SQL generated (notice I said the *same SQL* -- this does not mean that the results will be the same).
- **No Template Haskell** - Beam does not make use of Template Haskell. Rather, it uses several advanced extensions, such as `DeriveGeneric`, `DeriveDataTypeable`, `TypeFamilies`, `MultiParamTypeClasses`, etc. Although this means that Beam is not 100% Haskell 2010, these syntax extensions embody the "spirit of Haskell" much better than Template Haskell. Additionally, unlike Template Haskell, these extensions are not so much tied to the structure of GHC.

## Defining our first Beam database schema

In Beam, everything is done via plain old Haskell data types. Let's define a simple todo list database in Beam, and then use this schema to make queries on a SQLite3 database. We begin by defining type-level names for our columns. We will have two tables in our schema: one for todo lists and one for todo items. A todo list will have two columns: a name and a description. A todo item will have three: a name, a description, and a due date. We will need to use the `DeriveGeneric` and `DeriveDataTypeable` extensions in order to allow our names to play nicely with Beam.

```haskell
{-# LANGUAGE DeriveGeneric, DeriveDataTypeable #-}
import GHC.Generics
import Data.Typeable
data Name = Name deriving (Generic, Typeable)
data Description = Description deriving (Generic, Typeable)
data DueDate = DueDate deriving (Generic, Typeable)
```

Using these names we can define our Haskell-level data types for our tables. Notice that these definition are given in plain old Haskell. No crufty Template Haskell DSLs here!

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

All we have left is to define a few instances, so our type is Beam compatible. These instances will be auto-generated for us with sensible Beam defaults, so we can leave the instance declarations empty. Later on, if we wanted to customize exactly how each field and table is mapped to SQL (for example, to change the SQL name of a table), we would populate these instances. The rules for which instances we need to instantiate are easy. They are:

1. For each Haskell data type that we want to map to a SQL table, we need to instantiate the `Table` type class.
2. For each name for a `Column` in the table, we need to instantiate a `Field` instance referencing both the table and the name.
3. For each name for a `ForeignKey` in the table, we need to instantiate a `Reference` instance refererncing both the table and the name.

Applying these rules is straightforward. First, we apply rule 1 to generate two instances:

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

We're almost ready to use these types in a real database. First though, we need to create a `Database` object with the right tables.

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

We're using the `queryList` function to get our results as a list. The normal `query` function returns a `Source` from the `conduit` package, which is usually easier and safer to work with when writing an application, but is not as intuitive when working on the command line.

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