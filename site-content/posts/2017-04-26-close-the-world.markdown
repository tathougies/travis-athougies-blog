---
title: Close the world
author: Travis Athougies
tags: "haskell"
published: true
---

Haskell's polymorphism is awesome for writing generic code.

In particular, Haskell's typeclass mechanism allows you to write code that works
over any type that implements certain functionality. Haskell's GADT support
allows you to hold references to values where you know nothing about the type,
except the functionality it exports. The `-XConstraintKinds` feature allows us
to be polymorphic over not only types but the constraints on those types as
well. For example, the following type carries around the constraint parameter to
the type and exposes it when the constructor is matched upon.

```haskell
data Dict c where
  Dict :: c => Dict c
```

Another awesome feature is Haskell's `Typeable` mechanism, which allows us to
'forget' about types at compile time and recover them safely at run-time. In
particular, you can do something like

```haskell
data MyDynamic where
  MyDynamic :: Typeable a => a -> MyDynamic
```

Here, `MyDynamic` can be used to store any Haskell type. When we want to use the
value, we can use `Data.Typeable.cast` to get a value of a certain type, but
*only if that value had the type we wanted to begin with*. This is type-safe, so
there's no risk of seg faulting. This works because GHC always knows the type
passed to `MyDynamic` when the value is created. The `Typeable a` constraint in
the constructor causes GHC to not only store the value of type `a` in the
constructor, but also the 'dictionary' representing the implementation of the
`Typeable` instance for `a`. 

Now, for example, if we had a list of `MyDynamic`

```haskell
-- The explicit type annotations are necessary so GHC knows what 'Typeable' dictionary to include
myList :: [MyDynamic]
myList = [ MyDynamic (5 :: Int), MyDynamic (6 :: Integer), MyDynamic ("Hello world" :: String), MyDynamic (Nothing :: Maybe Int) ]
```

We can define a function

```haskell
only :: forall a. Typeable a => [MyDynamic] -> [a]
only = mapMaybe (\(MyDynamic x) -> cast x)
```

Now, we can ask GHC for only the `Int` values in `myList`

```haskell
only @Int myList == [ 5 ] 
```

Or, the `Integer`

```haskell
only @Integer myList = [ 6 ]
```

## Enter polymorphism

This is awesome, but suppose you wanted to write a function that could increment
a list containing values of any type that implemented `Num`. You would like this
list to be able to hold different types. For example, perhaps you had the
following list.

```haskell
myNumList :: [ MyDynamic ]
myNumList = [ MyDynamic (1 :: Int), MyDynamic (2 :: Double), MyDynamic (3 :: Integer) ]
```

We would like to write.

```haskell
incrementAll :: [MyDynamic] -> [MyDynamic]
incrementAll = map (\(MyDynamic a) -> MyDynamic (maybe a (+1) (cast a)))
```

However, if you compile this, you will get an error about ambiguous types. You
could try giving a type to `1` or `cast`, but, if you do, this function will
only increment values of that type. For example, if you substituted `cast a` for
`cast a :: Maybe Int`, `incrementAll myNumList` would yield `[MyDynamic (2 ::
Int), MyDynamic (2 :: Double), MyDynamic (3 :: Integer)]`. That's not very
useful.

This is moderately annoying. It seems like `Typeable` ought to give us enough
information to figure this out, but alas, `Typeable` doesn't magically make
Haskell a dynamic language.

It would be awesome if we could write a function like `cast` that let us operate
on a value as long as its type fulfilled certain constraints. That is,

```haskell
withConstraint :: forall (c :: * -> Constraint) r. MyDynamic -> r -> (forall a. (Typeable a, c a) => a -> r) -> r
```

This function says takes a constraint over a single type parameter, a
`MyDynamic`, a default result, and a function that can produce an `r`, given any
`a` that satisfies the constraint. It returns the first `r` if the value in the
`MyDynamic` doesn't satisfy constraint `c`. If the value does satisfy `c`, then
it passes that value to the given function and returns that value instead. If we
had `withConstraint`, we can write `incrementAll` as

```haskell
incrementAll :: [ MyDynamic ] -> [ MyDynamic ]
incrementAll = map (\d -> withConstraint @Num d d (\x -> MyDynamic (x + 1)))
```

## The world is too open

It turns out we can't write `withConstraint` in its general form. For one thing,
the set of instances available can change at run-time. Its not common, but
Haskell code could potentially load a plugin at run-time. Thus, an instance for
`Num x` may be introduced at run-time. Additionally, such run-time loading could
introduce incoherent instances. Thus, any `withConstraint` function would have
to be written in `IO`, so it could observe this global state.

However, the following function can still be written.

```haskell
forMyNums :: MyDynamic -> r -> (forall a. (Typeable a, Num a) => a -> r) -> r
forMyNums (MyDynamic x) def fn =
  fromMaybe def $ tryInt <|> tryDouble <|> tryInteger
  where
    tryInt = case cast x of
               Nothing -> Nothing
               Just (x :: Int) -> Just (fn x) -- Here, GHC knows 'x' has type 'Int', and it knows a 'Num Int' instance exists
    tryDouble = case cast x of
                  Nothing -> Nothing
                  Just (x :: Double) -> Just (fn x)
    tryInteger = case cast x of
                   Nothing -> Nothing
                   Just (x :: Integer) -> Just (fn x)
```

We can simplify this a bit more too.

```haskell
tryNum :: forall a x. (Typeable x, Typeable a, Num a) => x -> Maybe a
tryNum x = cast x

forMyNums :: MyDynamic -> r -> (forall a. (Typeable a, Num a) => a -> r) -> r
forMyNums (MyDynamic x) def fn =
  fromMaybe def $ fmap fn (tryNum @Int x) <|> fmap fn (tryNum @Double x) <|> fmap fn (tryNum @Integer x)
```

And we can generalize even this!

```haskell
tryHead :: forall c a x. (Typeable x, Typeable a, c a) => x -> Maybe a
tryHead x = cast x

withHead :: forall c r. (c Double, c Int, c Integer) => MyDynamic -> r -> (forall a. (Typeable a, c a) => a -> r) -> r
withHead (MyDynamic x) def fn =
  fromMaybe def $ fmap fn (tryHead @c @Int x) <|> fmap fn (tryHead @c @Double x) <|> fmap fn (tryHead @c @Integer x)
```

Now `forMyNums = withHead @Num`. Of course, this only works if our types in the
`MyDynamic` are `Int`, `Double`, or `Integer`. If we were to add a `MyDynamic (x
:: Float)`, it would not be incremented. However, it does show something
important: *if we limit ourselves to certain types, what we wanted becomes
possible*.

Additionally, what should be immediately clear here is that adding another type
to our universe of types, is very mechanical. To add `Float`, just add another
alternative in our `(<|>)` chain. In fact, it seems like these cast attempts
combine monoidally.

## The `closed-world` package

The [`closed-world`](https://github.com/tathougies/closed-world) package takes
this idea to its logical extreme. It provides a type `Universe` that can hold
any set of coherent instances. Not only can `Universe` hold concrete instances
as we showed above, it can also hold instance constructors. For example, while
`Show Int` is a concrete instance `Show (a, b)` is an 'instance constructor'. It
says that, given an instance for `Show a` and one for `Show b`, I can construct
such an instance for `Show (a, b)`.

Because `Universe`s are like any Haskell value, it can only be constructed at
some pure point in the program's execution. At any particular point in a Haskell
program, the compiler knows that the set of applying instances is indeed
coherent (unless some very nasty compiler extensions are being turned on). This
gets around the problems identified above where the 'global' set of instances
may be incoherent.

`closed-world` provides a function `withHead` that looks like what we wrote
above, except it also takes a `Universe` parameter identifying the full set of
instances to use.

```haskell
withHead :: forall cs a. Typeable cs => Universe -> a -> (cs => a) -> a
```

Now, if you have a value

```haskell
myList :: [MyDynamic]
myList = [ MyDynamic (1 :: Int)
         , MyDynamic ("hello world" :: String)
         , MyDynamic (("hello", 3) :: (String, Int))
         , MyDynamic (("hello", "world") :: (String, String))
         , MyDynamic (Nothing :: Maybe Int)
         , MyDynamic (Just 34 :: Maybe Int)
         , MyDynamic (Just ("hello, world", 4) :: Maybe (String, Int))
         , MyDynamic ([1,2,3,4,5] :: [Int])
         , MyDynamic (["hello", "world"] :: [String]) ]
```

You can specify a set of instances to use (you'll need Template Haskell)

```haskell
showIntInstance :: Universe
showIntInstance = $(mkUniverse [d| instance Show Int |])

showCharInstance :: Universe
showCharInstance = $(mkUniverse [d| instance Show Char |])

-- Universes combine monoidally
simpleShowInstances :: Universe
simpleShowInstances = showIntInstance <> showCharInstance

-- mkUniverse can also take a whole set of instance declarations
compoundShowInstances :: Universe
compoundShowInstances = $(mkUniverse [d| instance Show a => Show [a] 
                                         instance (Show a, Show b) => Show (a, b)
                                         instance Show a => Show (Maybe a) |])
```

And now,

```haskell
fmap (\(MyDynamic (x :: a)) -> withHead @(Show a) u Nothing (Just $ show x)) myList
-- Yields [Just "1",Just "\"hello world\"",Just "(\"hello\",3)",Just "(\"hello\",\"world\")",Just "Nothing",Just "Just 34",Just "Just (\"hello, world\",4)",Just "[1,2,3,4,5]",Just "[\"hello\",\"world\"]"]
```

`closed-world` is available on github, and there is more documentation there.
I'll cover more about the internals in a later post.
