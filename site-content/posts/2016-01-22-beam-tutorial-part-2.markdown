---
title: Beam tutorial (part 2)
author: Travis Athougies
tags: "haskell, beam"
published: true
---







This is the second part in my tutorial on the beam database library. This tutorial assumes you've read through the [first tutorial](post:2016-01-21-beam-tutorial-1) already. A literate haskell version of this exact tutorial can be found on [GitHub](https://github.com/tathougies/beam/blob/master/Doc/02-NextSteps.lhs).

## Introduction

In the previous tutorial, we created a simple database with one table. We then used
the beam interface to add entities into that table and query them. In this tutorial, we'll see how
to update and delete rows and how to establish and query relations between tables.

We'll then delve deeper into queries to see how to create queries that return multiple tables

## Where we left off

When we last left off, we had a database with one table, `UserT`. We duplicate all the work up until
this point from the last tutorial here.

```haskell
{-# LANGUAGE StandaloneDeriving, TypeSynonymInstances, FlexibleInstances, TypeFamilies, DeriveGeneric, OverloadedStrings #-}
module Main where

import Database.Beam
import Database.Beam.Backend.Sqlite3

import Control.Monad

import Data.Text (Text)
import Lens.Micro

data UserT f = User
             { _userEmail     :: Columnar f Text
             , _userFirstName :: Columnar f Text
             , _userLastName  :: Columnar f Text
             , _userPassword  :: Columnar f Text }
              deriving Generic

type User = UserT Identity
deriving instance Show User

instance Table UserT where
    data PrimaryKey UserT f = UserId (Columnar f Text) deriving Generic
    primaryKey = UserId . _userEmail

type UserId = PrimaryKey UserT Identity
deriving instance Show UserId
```

## Adding a related table

The users in our simple e-commerce application would like to ship orders to their homes. Let's build
an addresses model to allow users to add home addresses to their profile. Our table will store
United States addresses for now. An address in the United States consists of

  * one required house number and street line
  * an optional apartment/suite number line
  * a required city
  * a required 2-letter state/territory code
  * one 5-digit ZIP code

Let's build the `AddressT` table. `AddressT` will follow a similar formula to `UserT`, but it will
contain a reference to a `UserT` table.

```haskell
data AddressT f = Address
                { _addressId    :: Columnar f AutoId
                , _addressLine1 :: Columnar f Text
                , _addressLine2 :: Columnar f (Maybe Text)
                , _addressCity  :: Columnar f Text
                , _addressState :: Columnar f Text
                , _addressZip   :: Columnar f Text

                , _addressForUser :: PrimaryKey UserT f }
                  deriving Generic
type Address = AddressT Identity
deriving instance Show Address

instance Table AddressT where
    data PrimaryKey AddressT f = AddressId (Columnar f AutoId) deriving Generic
    primaryKey = AddressId . _addressId
```

The lines of particular interest are the declarations for `_addressId` and `_addressForUser`.

The `_addressId` field is declared with type `AutoId`. `AutoId` is defined in `Database.Beam.Schema.Fields` as

```haskell
data AutoId = UnassignedId
            | AssignedId !Int
```

When created in the database, `AutoId` defaults to a field that will automatically assign itself a
unique number when a NULL is written to it. Many people use such a field to create an easy primary
key. Note that depending on your backend, such a field may only be possible if it is also declared
as the primary key, which it is in our example.

The second field of interest is `_addressForUser`, which is declared as a `PrimaryKey UserT f`. This
pulls in all the columns necessary for referencing a `UserT`. Later, we'll also see how beam can use
the field to automatically create JOINs.

### Specifiying Field Options

Above, we gave two requirements for the state and ZIP code fields. We said that state must be a
2-digit state/territory code and ZIP must be at most 4 digits. This means we'd want to declare the
state field as a `CHAR(2)` in SQL, and the ZIP a `VARCHAR(5)`. By default, `Text` fields are
declared `VARCHAR`. As you may have guessed, beam provides a mechanism for changing the default.

We can declare options for a beam table by overriding the `tblFieldSettings` value in `Table`. This
value defaults to `defTblFieldSettings` and is constructed from the generic representation of the
table. If we use GHCi, we see that the type of `tblFieldSettings` is `DatabaseSettings
table`. `DatabaseSettings table` is a type synonym for `table (TableField table)`. `TableField
table` is another column tag (similar to `Identity`) that lets us define options on each table
field. By default, each table field of type `Columnar (TableField table) x` will now hold a value of
type `TableField table x`.

Because `TableField`s are deeply nested structures, it's easiest to use lenses to modify them. Beam
does not depend on the `lens` library, but lenses are plain old polymorphic Haskell data types, so
we can still make use of them without that library. In this example, we pulled in the
`microlens` library, which contains many common `lens` functions, but which does not use Template
Haskell. `microlens` is 100% compatible with `lens`, and beam is agnostic when it comes to choice of lens library.

Nevertheless, without Template Haskell we are typically left without any easy way to derive
lenses. Luckily for us, we'll see how beam let's us automatically derive these lenses. For now,
let's just assume the following lenses exist:

```haskell
addressStateC :: Lens' (AddressT (TableField AddressT)) (TableField AddressT Text)
addressZipC :: Lens' (AddressT (TableField AddressT)) (TableField AddressT Text)
```

We also have the following lenses of interest to interface with `TableField table`:

```haskell
fieldName :: Lens' (TableField table ty) Text
fieldSchema :: Lens (TableField table a) (TableField table b) (FieldSchema a) (FieldSchema b)
```

A `FieldSchema a` is a record type that contains information on how to
serialize and deserialize the particular column. Beam automatically
chooses a default field schema for you using the
`HasDefaultFieldSchema` type class.

For our purposes, we need only look at the `textSchema` column schema
constructor. It takes in a sum type `CharOrVarchar` and produces the
appropriate schema for a `Text` field.

```haskell
textSchema :: CharOrVarchar -> FieldSchema Text

data CharOrVarchar = Char (Maybe Int)
                   | Varchar (Maybe Int)
                     deriving Show
```

Let's use these lenses and this knowledge to modify the default storage type for these two
fields. We're going to override the `tblFieldSettings` value in the `Table AddressT`
instantiation. We can use `defTblFieldSettings` to get the automatically derived settings from Beam,
so that we can only override the parts we're interested in.

```haskell
    tblFieldSettings = defTblFieldSettings
                       & addressStateC . fieldSchema .~ textSchema (Char (Just 2))
                       & addressZipC . fieldSchema .~ textSchema (Varchar (Just 5))
```

This completes our `Table AddressT` instantiation.

### Lenses for free

Above we presumed the existence of lenses that let us access the `Columnar (TableField Address) x`
members of our table type. Here, we'll see how we can make these lenses using generics. First we
start with the finish product, and then explain what's going on.

```haskell
Address (LensFor addressIdC)
        (LensFor addressLine1C)
        (LensFor addressLine2C)
        (LensFor addressCityC)
        (LensFor addressStateC)
        (LensFor addressZipC)
        (UserId (LensFor addressForUserIdC)) = tableConfigLenses
```

This is a pattern match at the top level. `tableConfigLenses` uses GHC's generics mechanism and a
special column tag to automatically replace all instances of `Columnar f x` in the data structure
with the `LensFor` newtype. Note how it even replaced the `Colunmar f x`s that were embedded in the
`_addressForUser` primary key field.

We can ask GHC for the types of the derived lenses. As a reminder, the type of a simple van
Laarhoven lens from a data structure `a` to a substructure `b` is

```haskell
type Lens' = forall f. Functor f => (b -> f b) -> a -> f a
```

We'd expect that the type of `addressZipC` is

```haskell
addressZipC :: Lens' (AddressT (TableField AddressT)) (TableField AddressT Text)
```

If we ask GHCi, we get

```
*NextSteps> :t addressZipC
addressZipC
:: Functor f =>
   (TableField AddressT Text -> f (TableField AddressT Text))
   -> AddressT (TableField AddressT)
   -> f (AddressT (TableField AddressT))
```

which is equivalent to the above.

## Working with relations

Now, let's see how we can add related addresses to our database. First, we'll define a type for our
new database and declare an instance of the database, using the default beam settings. We'll then
open up a connection for us to use in the rest of the tutorial.

```haskell
data ShoppingCartDb f = ShoppingCartDb
                      { _shoppingCartUsers         :: f UserT
                      , _shoppingCartUserAddresses :: f AddressT}
                        deriving Generic

instance Database ShoppingCartDb

shoppingCartDb :: DatabaseSettings ShoppingCartDb
shoppingCartDb = autoDbSettings

main :: IO ()
main = do beam <- openDatabaseDebug shoppingCartDb AutoMigrate (Sqlite3Settings "shoppingcart2.db")
          dumpSchema shoppingCartDb -- Just to see what it's like
```

Before we add addresses, we need to add some users that we can reference.

```haskell
          let [james, betty, sam] = [ User "james@example.com" "James" "Smith" "b4cc344d25a2efe540adbf2678e2304c" {- james -}
                                    , User "betty@example.com" "Betty" "Jones" "82b054bd83ffad9b6cf8bdb98ce3cc2f" {- betty -}
                                    , User "sam@example.com" "Sam" "Taylor" "332532dcfaa1cbf61e2a266bd723612c" {- sam -} ]
          beamTxn beam $ \(ShoppingCartDb usersT userAddressesT) ->
              mapM_ (insertInto usersT) [james, betty, sam]
```

Now that we have some `User` objects, we can create associated addresses. Let's give James one
address, Betty two addresses, and Sam none.

```haskell
          let addresses = [ Address UnassignedId "123 Little Street" Nothing "Boston" "MA" "12345" (pk james)

                          , Address UnassignedId "222 Main Street" (Just "Ste 1") "Houston" "TX" "8888" (pk betty)
                          , Address UnassignedId "9999 Residence Ave" Nothing "Sugarland" "TX" "8989" (pk betty) ]
```

Notice that we used the `pk` function to assign the reference to the `UserT` table. `pk` is a
synonym of the `primaryKey` function from the `Table` type class. It should be clear what's going
on, but if it's not, let's ask GHCi.

```
*NextSteps> pk (User "james@example.com" "James" "Smith" "b4cc344d25a2efe540adbf2678e2304c" {- james -} :: User)
UserId "james@example.com"
```

Notice also that we set `_addressId` to `UnassignedId`. As mentioned above, the `AutoId` type means
the addresses won't have an id until they're inserted into the database. This could be an issue if
we want to refer to the addresses in the future. After all, we cannot ask the database to search for
`UnassignedId`. Fortunately, the `insertInto` function returns the value of the newly inserted
row. We can use this to get the id assigned to the `Address`.

```haskell
          Success [jamesAddress1, bettyAddress1, bettyAddress2] <-
              beamTxn beam $ \(ShoppingCartDb usersT userAddressesT) ->
                  mapM (insertInto userAddressesT) addresses
```

Now we can print out the addresses to see that they were indeed assigned an id.

```haskell
          putStrLn "The inserted addresses are:"
          mapM_ (putStrLn . show) [jamesAddress1, bettyAddress1, bettyAddress2]
```

You should see output like the following, confirming that the addresses were assigned an id.

```
The inserted addresses are:
Address {_addressId = AssignedId 1, _addressLine1 = "123 Little Street", _addressLine2 = Nothing, _addressCity = "Boston", _addressState = "MA", _addressZip = "12345", _addressForUser = UserId "james@example.com"}
Address {_addressId = AssignedId 2, _addressLine1 = "222 Main Street", _addressLine2 = Just "Ste 1", _addressCity = "Houston", _addressState = "TX", _addressZip = "8888", _addressForUser = UserId "betty@example.com"}
Address {_addressId = AssignedId 3, _addressLine1 = "9999 Residence Ave", _addressLine2 = Nothing, _addressCity = "Sugarland", _addressState = "TX", _addressZip = "8989", _addressForUser = UserId "betty@example.com"}
```

## A note about queries

In the last tutorial, we saw how queries and list supported similar interfaces. Namely we saw how
`limit_` is like `take`, `offset_` like `drop`, `orderBy` like an enhanced `sortBy`, and `aggregate`
like an enhanced `groupBy`. This corresponded to the `LIMIT`, `OFFSET`, `ORDER BY`, and `GROUP BY`
SQL constructs. The missing SQL operation in this list is the `JOIN`, which computes the cartesian
product of two tables. In other words, a join between table `A` and table `B` results in a query of
pairs `(x, y)` for every `x` in `A` and every `y` in `B`. SQL joins can result in two-way,
three-way, four-way, etc. cartesian products.

Those familiar with lists in Haskell will note that there is an easy construct for taking n-ary
cartesian products over lists: the monad.

### The list monad

If we open GHCi, we can see this construct in action. Type the following into GHCi

```
*NextSteps> do { x <- [1,2,3]; y <- [4,5,6]; return (x, y); }
[(1,4),(1,5),(1,6),(2,4),(2,5),(2,6),(3,4),(3,5),(3,6)]
```

We get the two-way cartesian product of `[1,2,3]` and `[4,5,6]`. We can make the product arbitrarily long.

```
*NextSteps> do { w <- [10, 20, 30]; x <- [1,2,3]; y <- [4,5,6]; z <- [100, 200, 1]; return (x, y, z, w); }
[(1,4,100,10),(1,4,200,10),(1,4,1,10),(1,5,100,10),(1,5,200,10),(1,5,1,10), ... ]
```

We can also use `guard` from `Control.Monad` to limit the combinations that the list monad puts
together. For example, if we had the lists

```haskell
let usersList = [(1, "james"), (2, "betty"), (3, "tom")]
    addressesList = [(1, "address1"), (1, "address2"), (3, "address3")]
```

We can use `guard` to return all pairs of elements from `usersList` and `addressesList` that matched on their first
element. For example,

```haskell
*NextSteps> do { user <- usersList; address <- addressesList; guard (fst user == fst address); return (user, address) }
[((1,"james"),(1,"address1")),((1,"james"),(1,"address2")),((3,"tom"),(3,"address3"))]
```

### The query monad

As I claimed in the first tutorial, queries support many of the same interfaces and operations lists
do. It follows that queries also expose a monadic interface.

For example, to retrieve every pair of user and address, we can write the following query:

```haskell
          putStrLn "All pairs of users and addresses"
          Success allPairs <-
              beamTxn beam $ \(ShoppingCartDb usersT addressesT) ->
                queryList $
                do user <- all_ usersT
                   address <- all_ addressesT
                   return (user, address)
          mapM_ (putStrLn . show) allPairs
          putStrLn "----\n\n"
```

You'll get output like the following

```
Will execute SELECT `t0`.`email`, `t0`.`first_name`, `t0`.`last_name`, `t0`.`password`, `t1`.`id`, `t1`.`line1`, `t1`.`line2`, `t1`.`city`, `t1`.`state`, `t1`.`zip`, `t1`.`for_user__email` FROM  cart_users AS t0 INNER JOIN cart_user_addresses AS t1 with []
(User {_userEmail = "james@example.com", _userFirstName = "James", _userLastName = "Smith", _userPassword = "b4cc344d25a2efe540adbf2678e2304c"},Address {_addressId = AssignedId 1, _addressLine1 = "123 Little Street", _addressLine2 = Nothing, _addressCity = "Boston", _addressState = "MA", _addressZip = "12345", _addressForUser = UserId "james@example.com"})
(User {_userEmail = "james@example.com", _userFirstName = "James", _userLastName = "Smith", _userPassword = "b4cc344d25a2efe540adbf2678e2304c"},Address {_addressId = AssignedId 2, _addressLine1 = "222 Main Street", _addressLine2 = Just "Ste 1", _addressCity = "Houston", _addressState = "TX", _addressZip = "8888", _addressForUser = UserId "betty@example.com"})
...
----
```

Just like with lists we can also use a construct similar to guard to ensure that we only retrieve
users and addresses that are related. The `guard_` function takes in expression of type `QExpr s Bool`
which represents a SQL expression that returns a boolean. `QExpr s Bool`s support all the common
operators we have on regular `Bool`, except they're suffixed with a `.`. For example, where you'd
use `(&&)` on two Haskell-level `Bool`s, we'd use `(&&.)` on `QExpr`-level bools.

```haskell
          putStrLn "All pairs of users with their related addresses"
          Success usersAndRelatedAddresses <-
              beamTxn beam $ \(ShoppingCartDb usersT addressesT) ->
                  queryList $
                  do user@(User { _userEmail = userEmail }) <- all_ usersT
                     address@(Address {_addressForUser = UserId addressForUser}) <- all_ addressesT

                     guard_ (addressForUser ==. userEmail)
                     pure (user, address)
          mapM_ (putStrLn . show) usersAndRelatedAddresses
          putStrLn "----\n\n"
```

The output for this query is

```
Will execute SELECT `t0`.`email`, `t0`.`first_name`, `t0`.`last_name`, `t0`.`password`, `t1`.`id`, `t1`.`line1`, `t1`.`line2`, `t1`.`city`, `t1`.`state`, `t1`.`zip`, `t1`.`for_user__email` FROM  cart_users AS t0 INNER JOIN cart_user_addresses AS t1 WHERE `t1`.`for_user__email` == `t0`.`email` with []
(User {_userEmail = "james@example.com", _userFirstName = "James", _userLastName = "Smith", _userPassword = "b4cc344d25a2efe540adbf2678e2304c"},Address {_addressId = AssignedId 1, _addressLine1 = "123 Little Street", _addressLine2 = Nothing, _addressCity = "Boston", _addressState = "MA", _addressZip = "12345", _addressForUser = UserId "james@example.com"})
(User {_userEmail = "betty@example.com", _userFirstName = "Betty", _userLastName = "Jones", _userPassword = "82b054bd83ffad9b6cf8bdb98ce3cc2f"},Address {_addressId = AssignedId 2, _addressLine1 = "222 Main Street", _addressLine2 = Just "Ste 1", _addressCity = "Houston", _addressState = "TX", _addressZip = "8888", _addressForUser = UserId "betty@example.com"})
(User {_userEmail = "betty@example.com", _userFirstName = "Betty", _userLastName = "Jones", _userPassword = "82b054bd83ffad9b6cf8bdb98ce3cc2f"},Address {_addressId = AssignedId 3, _addressLine1 = "9999 Residence Ave", _addressLine2 = Nothing, _addressCity = "Sugarland", _addressState = "TX", _addressZip = "8989", _addressForUser = UserId "betty@example.com"})
```

Of course this is kind of messy because it involves manually matching the primary key of `User` with
the reference in `Address`. Alternatively, we can use the `references_` combinator to have Beam
automatically generate a `QExpr` expression that can match primary keys together.

```haskell
          putStrLn "All pairs of users with their related addresses (using references_)"
          Success usersAndRelatedAddressesUsingReferences <-
              beamTxn beam $ \(ShoppingCartDb usersT addressesT) ->
                  queryList $
                  do user <- all_ usersT
                     address <- all_ addressesT

                     guard_ (_addressForUser address `references_` user)
                     pure (user, address)
          mapM_ (putStrLn . show) usersAndRelatedAddressesUsingReferences
          putStrLn "----\n\n"
```

The debug output shows that we get the same query and same output as above.

You may have noticed that the joins up until now did not include a SQL `ON` clause. Instead we
joined the tables together, and then used the `WHERE` clause to filter out results we don't want. If
you'd like to use the `ON` clause to make the SQL clearer or save a line in your code, beam offers
the `related_` combinator to pull related tables directly into the query monad.

```haskell
          putStrLn "All pairs of users with their related addresses (using related_)"
          Success usersAndRelatedAddressesUsingRelated <-
              beamTxn beam $ \(ShoppingCartDb usersT addressesT) ->
                  queryList $
                  do address <- all_ addressesT
                     user <- related_ usersT (_addressForUser address)
                     pure (user, address)
          mapM_ (putStrLn . show) usersAndRelatedAddressesUsingRelated
          putStrLn "----\n\n"
```

The output shows us that the correct `ON` clause has been generated, and you can verify that the
results are the same.

```
Will execute SELECT `t1`.`email`, `t1`.`first_name`, `t1`.`last_name`, `t1`.`password`, `t0`.`id`, `t0`.`line1`, `t0`.`line2`, `t0`.`city`, `t0`.`state`, `t0`.`zip`, `t0`.`for_user__email` FROM  cart_user_addresses AS t0 INNER JOIN cart_users AS t1 ON `t0`.`for_user__email` == `t1`.`email` with []
...
----
```

We can also query the addresses for a particular user given a `UserId`.

```haskell
          -- This is a contrived example to show how we can use an arbitrary UserId to fetch a particular user.
          -- We don't always have access to the full 'User' lying around. For example we may be in a function that
          -- only accepts 'UserId's.
          let bettyId = UserId "betty@example.com" :: UserId
          putStrLn "All addresses for 'betty@example.com'"
          Success bettysAddresses <-
              beamTxn beam $ \(ShoppingCartDb usersT addressesT) ->
                 queryList $
                  do address <- all_ addressesT
                     guard_ (_addressForUser address ==. val_ bettyId)
                     pure address
          mapM_ (putStrLn . show) bettysAddresses
          putStrLn "----\n\n"
```

Again the correct SQL and results are generated.

```
Will execute SELECT `t0`.`id`, `t0`.`line1`, `t0`.`line2`, `t0`.`city`, `t0`.`state`, `t0`.`zip`, `t0`.`for_user__email` FROM  cart_user_addresses AS t0 WHERE `t0`.`for_user__email` == ? with [SqlString "betty@example.com"]
Address {_addressId = AssignedId 2, _addressLine1 = "222 Main Street", _addressLine2 = Just "Ste 1", _addressCity = "Houston", _addressState = "TX", _addressZip = "8888", _addressForUser = UserId "betty@example.com"}
Address {_addressId = AssignedId 3, _addressLine1 = "9999 Residence Ave", _addressLine2 = Nothing, _addressCity = "Sugarland", _addressState = "TX", _addressZip = "8989", _addressForUser = UserId "betty@example.com"}
----
```

## Updates and deletions

So far we've only seen how to insert data and query it. There are two other SQL operations that we
have not covered: updates and deletions. Beam has full support for these manipulations as well.

There are four functions that we're interested in: `save`, `updateWhere`, `deleteFrom`, and
`deleteWhere`. Let's look at their type signatures to see how they work.

```haskell
save :: (MonadIO m, Table tbl) => DatabaseTable db tbl -> tbl Identity -> BeamT db m ()
updateWhere :: (MonadIO m, Table tbl) => DatabaseTable db tbl -> (tbl (QExpr s) -> tbl SetExpr) -> (tbl (QExpr s) -> QExpr s Bool) -> BeamT db m ()
deleteFrom :: (MonadIO m, Table tbl) => DatabaseTable db tbl -> tbl Identity -> BeamT db m ()
deleteWhere :: (MonadIO m, Table tbl) => DatabaseTable db tbl -> (tbl (QExpr s) -> QExpr s Bool) -> BeamT db m ()
```

Note that in the declarations above `QExpr` is the type of all expressions allowed in SQL, and the
`s` parameter is a threading parameter that prevents `QExpr`s from being used in inappropriate
contexts, similar to how the `s` in `ST s` allows you to use mutable data in a pure fashion.

### Updates

Let's first look at updating passwords given a `User`. For this we can use the `saveTo`
function. Suppose James wants to change his password to the md5 hash of "supersecure", which is
`52a516ca6df436828d9c0d26e31ef704`. We have a `User` object representing James so we can simply call
`saveTo` on the update value to update the corresponding record in the database.

```haskell
          putStrLn "Updating james' password"
          beamTxn beam $ \(ShoppingCartDb usersT addressesT) ->
              saveTo usersT (james { _userPassword = "52a516ca6df436828d9c0d26e31ef704" })
          Success newPassword <- beamTxn beam $ \(ShoppingCartDb usersT addressesT) ->
                                 getOne $ do james <- lookup_ usersT (val_ (UserId "james@example.com" :: UserId))
                                             return (_userPassword james)
          putStrLn ("Verified that james's new password is " ++ show newPassword)
          putStrLn "----\n"
```

When this runs, you'll see that the password pulled from the database matches the one we just saved.

```
Updating james' password
Will execute UPDATE cart_users SET email=?, first_name=?, last_name=?, password=? WHERE email == ? with [SqlString "james@example.com",SqlString "James",SqlString "Smith",SqlString "52a516ca6df436828d9c0d26e31ef704",SqlString "james@example.com"]
Will execute SELECT `t0`.`password` FROM  cart_users AS t0 WHERE ? == `t0`.`email` with [SqlString "james@example.com"]
Verified that james's new password is Just "52a516ca6df436828d9c0d26e31ef704"
```

This works great, but `saveTo` requires that we have the whole `User` object at our
disposal. Additionally, you'll notice that it causes every field to be set in the `UPDATE`
query. Typically, this doesn't matter, but sometimes we'd like to update fewer fields, multiple
rows, or use criteria other than a primary key match. The `updateWhere` function offers us the full
functionality of SQL `UPDATE`s.

To illustrate use of this function, let's suppose the city of "Sugarland, TX" was renamed
"Sugarville, TX" and had its ZIP code changed to be "12345" citywide. The following beam command
will update all addresses in the old city to use the new name and ZIP code.

```haskell
          beamTxn beam $ \(ShoppingCartDb usersT addressesT) ->
              updateWhere addressesT (\address -> address { _addressCity = val_ "Sugarville"
                                                          , _addressZip  = val_ "12345" })
                                     (\address -> _addressCity address ==. val_ "Sugarland" &&.
                                                  _addressState address ==. val_ "TX")
```

This will execute the expected `UPDATE` statement

```
Will execute UPDATE cart_user_addresses SET city=?, zip=? WHERE city == ? AND state == ? with [SqlString "Sugarville",SqlString "12345",SqlString "Sugarland",SqlString "TX"]
```

We can confirm that the address was updated by reading back the database's version of Betty's second
address.

```haskell
          putStrLn "\nChecking betty's second address to ensure it's updated to Sugarville, TX"
          Success address <- beamTxn beam $ \(ShoppingCartDb usersT addressesT) ->
                             getOne (lookup_ addressesT (val_ (primaryKey bettyAddress2)))
          putStrLn ("Got new address " ++ show address ++ "\n")
```

### Deletions

Now suppose that Betty has decided to give up her place in Houston. We can use the `delete` function
to remove a row if we have that row's primary key.

```haskell
          putStrLn "Deleting betty's first address"
          beamTxn beam $ \(ShoppingCartDb usersT addressesT) ->
              deleteFrom addressesT (pk bettyAddress1)
```

Beam executes an appropriate `DELETE` statement, using the primary key to reference the table.

```
Deleting betty's first address
Will execute DELETE FROM cart_user_addresses WHERE id == ? with [SqlInteger 2]
```

Just for fun, let's remove all users named Sam. After all, they don't have any addresses stored!

```haskell
          putStrLn "Deleting Sam"
          beamTxn beam $ \(ShoppingCartDb usersT addressesT) ->
              deleteWhere usersT (\user -> _userFirstName user ==. val_ "Sam")
          pure ()
```

Again, beam produces the SQL we'd expect

```
Deleting Sam
Will execute DELETE FROM cart_users WHERE first_name == ? with [SqlString "Sam"]
```

## Conclusion

In this tutorial we created our first beam relationship. We saw how to use `tableConfigLenses` and
the `microlens` library to change the default storage options beam chose for us. We used the monadic
tquery interface to write queries that used SQL joins, and we saw how beam makes it easy to
tautomatically pull related tables into our queries. Finally we used the `updateWhere`, `saveTo`,
`deleteWhere`, and `deleteFrom` functions to update and delete rows in our tables.

At this point, we've covered enough of the beam interface to start writing interesting
programs. Take some time to explore beam and create your own databases. For more information on the
Beam Query API, including all the combinators you can use for `QExpr`s, see the haddock
documentation for `Database.Beam.Query`. If you're interested in all the different field types
supported by Beam, see the module source for `Database.Beam.Fields`.

Until next time!

As always, if you have any questions, feel free to e-mail me at travis@athougies.net. Bugs and
patches should go to [GitHub](https://github.com/tathougies/beam).
