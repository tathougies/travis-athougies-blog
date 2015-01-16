---
title: Beam - A typesafe Haskell database interface
author: Travis Athougies
tags: haskell, database, types
---

I just uploaded a new package to github. It's called *Beam* and it's a type-safe database interface
for Haskell.

Haskell and its community value type-safety and elegant solutions to complex problems. However,
current Haskell database interface libraries (like Persistent and HaskellDB) are anything but. Both
make extended use of Template Haskell. Although a useful language feature, Template Haskell suffers
from several disadvantages, namely its complexity and lack of type safety.

What do these libraries mainly use Template Haskell for? Usually, it's for deriving instances and
generating boiler-plate code. However, Template Haskell isn't the only Haskell feature that lets us
get rid of this tedious boiler-plate. Several new GHC extensions, such as Generics, Type Families,
and Default Signatures allow us to write most of what these languages would use Template Haskell for
in Haskell itself. The best part is that we can do this without introducing crufty metaprogramming
or unintuitive DSLs. Instead, we rely on simple, intuitive Haskell syntax.

## Defining our first Beam database schema

In Beam, everything is done with plain old Haskell data types. To see an example of how we would
define a Schema in Beam, let's define a simple one-table database for a todo-list. Let's look at the
code before we review it line-by-line. Since there is no Template Haskell involved, this code can be
typed directly into GHCi.

Make sure to install Beam via Cabal:
```bash
cabal install beam
```

In either case, make sure to enable the `DeriveDataTypeable` and `DeriveGeneric` extensions.

```haskell
import Database.Beam

import Data.Typeable
import GHC.Generics

data Name = Name deriving (Generic, Typeable)
data Description = Description (Generic, Typeable)
data DueDate = DueDate deriving (Generic, Typeable)

data TodoListItemTable = TodoListItemTable (TextField Name)
                                           (TextField Description)
                                           (DateTimeField DueDate)
                         deriving (Generic, Typeable)
instance Table TodoListItemTable
instance Field TodoListItemTable Name
instance Field TodoListItemTable Description where
    fieldSettings _ _ = TextFieldSettings (Varchar (Just 100))
instance Field TodoListItemTable DueDate

data TodoListDatabase = TodoListDatabase (TableSchema TodoListItemTable)
                        deriving (Generic, Typeable)
instance Database TodoListDatabase
```

So what does this code do? Let's step through it line-by-line (skipping the imports).

```haskell
data Name = Name deriving (Generic, Typeable)
data Description = Description (Generic, Typeable)
```

Firstly, we have to define the field names that we will be using. Field names are simple unit
datatypes (unit means they have one constructor and no arguments). We usually name both the type and
constructor the same thing. This simplifies the code because it means we can reference the field by
the same name whether we're using it as data or as a type. The `deriving` clauses are to derive the
datatypes that Beam needs to automatically generate necessary instances for the classes. We can also
derive these instances ourselves, but Beam can derive an instance for any type (isomorphic to unit)
that we're interested in.

The next part
```haskell
data TodoListItemTable = TodoListItemTable (TextField Name)
                                           (TextField Description)
                         deriving (Generic, Typeable)
instance Table TodoListItemTable
instance Field TodoListItemTable Name
instance Field TodoListItemTable Description where
    fieldSettings _ _ = TextFieldSettings (Varchar (Just 100))
```
defines the schema for our first table, which holds information about todo list items in our
database. Each item has two text fields, one named `Name` and the other named
`Description`. In the type signature, `Name` and `Description` are phantom types, meaning that
they're not used in any data constructor for `TextField`. In fact, the definition for `TextField`
looks like
```haskell
data TextField name = TextField Text
```
However, this let's us reference this field by name simply by using the `Name` data constructor. For
example, type the following into GHCi
```haskell
> let x = TodoListItemTable (TextField "list 1") (TextField "This is an important task")
> getField x Name
"list 1"
```
By examining the type of `TodoListItemTable`, GHC was able to lookup the field named `Name` simply by
the type of `TodoListItemTable`! No Template Haskell necessary!

After the table declaration, there are a few `instance` declarations. These instances are derived
automatically by us thanks to the `Generic` and `Typeable` instances. They can be used to set
options about how the tables and fields are mapped to database-level objects. For example, the
`fieldSettings` definition in the `Field TodoListItemTable Description` instance instructs
`TextField` to use `VARCHAR(100)` as the underlying SQL type for this field.

Finally, the last part
```haskell
data TodoListDatabase = TodoListDatabase (TableSchema TodoListItemTable)
                        deriving (Generic, Typeable)
instance Database TodoListDatabase
```
defines a one table Beam database, consisting solely of the schema for `TodoListItemTable`. Again, the
`Database` instance is automatically derived.

## Tying it all together

Okay, now let's take it for a test drive. Currently, Beam only has support for one backend,
sqlite. Let's try creating our first Beam database. The following command will open a new Sqlite3
database and migrate it to our current schema. If you've been following the tutorial in a file, make
sure to import it into GHCi. Also, make sure the `OverloadedStrings` extension is enabled (run `:set
-XOverloadedStrings` in GHCi).
```haskell
> beam <- openDatabase (Proxy :: Proxy TodoListDatabase) (Sqlite3Settings "mydatabase.db")
Performing CreateTable TodoListItemTable
Will run SQL:
CREATE TABLE TodoListItemTable (id INTEGER PRIMARY KEY, Name VARCHAR, Description VARCHAR, DueDate DATETIME)
Done...
> runInsert (TodoListItemTable (TextField "item 1") (TextField "an important event")) beam
Will run SQL: INSERT INTO TodoListItemTable VALUES (NULL, "item 1", "an important event")
> runInsert (TodoListItemTable (TextField "item 2") (TextField "another important event")) beam
Will run SQL: INSERT INTO TodoListItemTable VALUES (NULL, "item 2", "another important event")
> runQuery (all_ (of_ :: TodoListTable) `where_` (\todoListTable -> (todoListTable # Name) ==$ StrE "item 1")) beam
Will execute SELECT * FROM TodoListTable AS t0 WHERE 1 AND `t0`.`Name` == "item 1"
Results: [[SqlByteString "1",SqlByteString "item 1",SqlByteString "an important event"]]
```
