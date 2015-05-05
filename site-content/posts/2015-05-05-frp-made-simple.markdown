---
title: FRP made easy!
author: Travis Athougies
tags: haskell
---

Functional Reactive Programming refers to a declarative way to write eventful program in functional
languages like Haskell. While the concepts  behind FRP are straightforward to grasp, the actual
implementations are not.

Libraries like sodium, reactive-banana, elerea, and yampa share one thing in common: large code
bases that are not easily understood by beginners. In this blog post, I will develop a sodium-like
FRP implementation for Haskell in under 200 lines. Note that this is possible by eschewing any
concerns over performance or memory usage, so this library would probably not be practical to use in
any live code.

## A brief recap of FRP

FRP libraries usually consist of two primitives: `Event` and `Behavior`. An `Event` refers to values
which only occur at certain times, and which are lost after the moment they occur. You can think of
an `Event a` as a list of pairs of times `t` and values `a`: `[(t, a)]`. A `Behavior` is a
continuous value. Here, continuous means that a `Behavior` has a defined value for any given time
(not the mathematical definition!). We can think of a `Behavior a` as a function from a time `t` to
the value `a`.

## First steps

The first thing we will do is define a monad for all of our FRP computations. Our FRP implementation
will also allow us to execute arbitrary `IO` actions in response to events (much like the `execute`
primitive in Sodium), so we will need a separate monad to represent what happens "in the moment."
Right now, we'll just make both of these monads aliases for `IO`.

Also, before we start, the final library we will end up writing is available on [GitHub.](https://github.com/tathougies/react.hs)

```haskell
type React = IO
type Moment = IO
```

In many FRP implementations, `Behavior` actually isn't anything more than a step function whose
value can only be updated by events. Therefore, we'll start by defining our `Event` type. The only
really important thing about events is that we be able to subscribe and unsubscibe from them.

```
type RegisterEventListener a = (a -> Moment ()) -> IO (IO ())
newtype Event a = Event { _eventRegisterListener :: RegisterEventListener a }
```

First, we define the `RegisterEventListener` type. Functions of this type can be used to register a
function in the `Moment` monad which will be called whenever a new value (of type `a`) of the event
is produced. The listener must be registered in the `IO` monad, and the registration function
returns another monadic action of type `IO ()`. This function is used to deregister the listener
that has been added.

`Event a` fills the requirements for both a `Monoid` and a `Functor`. For the `Monoid` instance,
`mempty` is the Event that never fires, and `mappend` takes two `Event a`'s and returns a new `Event
a` that fires whenever either of the first two fires.

```haskell
instance Monoid (Event a) where
  mempty = never
  mappend = merge

never :: Event a
never = Event (\_ -> return (return ()))

merge :: Event a -> Event a -> Event a
merge a b = Event (\listener -> do
                     unregisterA <- _eventRegisterListener a listener
                     unregisterB <- _eventRegisterListener b listener
                     return (unregisterA >> unregisterB))
```

Let's look at each function individually. First, let's consider the `mempty` or `never`
functions. Whenever a listener tries to register with this event, the listener is totally ignored
and the deregistration function returned does nothing. This means that the listener will never be
called (i.e., the event will never fire), which is the behavior we wanted.

The `mappend` or `merge` function take two events and returns a new one. When a listener tries to
register with the new event, the listener is in fact registered with both events. Thus, the listener
will be called whenever either `a` or `b` fire, which is the behavior we wanted. The deregistration
function deregisters the listener from both `a` and `b`.

Now for the `Functor` instance. The `Functor` instance for event allows us to apply functions to the
value contained inside an event in order to a get a new dependent event. I've included a type
signature to remind the reader of the type of the `fmap` function for `Event`'s.

```haskell
instance Functor Event where
  fmap :: (a -> b) -> Event a -> Event b
  fmap f eb = Event (\listener -> _eventRegisterListener eb (listener . f))
```

In `fmap`, we translate our listening function to be able to listen to the original event, and then
subscribe it to that. Recall that `eb` has type `Event b`, so `_eventRegisterListener eb` has type
`(b -> Moment ()) -> IO (IO ())`. The `listener` function we've been supplied has type `a -> Moment
()`, but `listener . f` has type `b -> Moment()`, so we can use it as the new listener for `eb`.

## Getting behaviors to behave

As we said before, although we can think of `Behavior`s as continuous-time properties, in reality,
our FRP implementation will treat them as stepper functions that must be updated by an
event. Therefore, every `Behavior` will need to be tied with an event that will fire whenever the
behavior is updated. Secondly, although the value of `Behavior` is impure (it changes through time),
its value is well defined given a point in time. Therefore, we should be able to access it through
`Moment`. Thus, our `Behavior` type will also have to support a way to get at its current value.

It's easy to combine these requirements into a `Behavior` data type, which consists of an event
which fires on updates and a function to get the current value.

```haskell
data Behavior a = Behavior
                { _behaviorUpdates :: Event ()
                , _behaviorGetValue :: Moment a }
```

It turns out that `Behavior` is an `Applicative`. The `pure` function for `Behavior` will return a
`Behavior` whose value does not change in time. The `(<*>)` function will return a `Behavior` whose
value at any given point in time is the current value of the first `Behavior` applied to the current
value of the second. I've included type signatures for convenience.

```haskell
import Control.Applicative

instance Applicative Behavior where
  pure :: a -> Behavior a
  pure a = Behavior { _behaviorUpdates = mempty
                    , _behaviorGetValue = return a}
  (<*>) :: Behavior (a -> b) -> Behavior a -> Behavior b
  bab <*> ba = Behavior { _behaviorUpdates = _behaviorUpdates bab <> _behaviorUpdates ba
                        , _behaviorGetValue = do ab <- _behaviorGetValue bab
                                                 a <- _behaviorGetValue ba
                                                 return (ab a) }
```

The `Behavior` returned by `pure a` is a `Behavior` that never updates (i.e., is constant) and whose
value always returns `a`. This fulfills our requirements for a constant `Behavior`.

The `Behavior` returned by `f <*> a` is a `Behavior` who updates whenever `f` or `a` update, and
whose value is the value of `f` applied to the value of `a`.

## Interfacing with the real world

So now we have `Event` and `Behavior` data type and methods to combine them with other `Event`'s and
`Behavior`'s respectively. What we don't have is any way to make an `Event` that actually fires, or
a `Behavior` that is not built from other `Behavior`'s. Let's implement this functionality to make
our library actually useful.

### Putting the 'R' in FRP

Most FRP implementations offer the user a way to create an event as well as an IO (or equivalent)
function that can be used to trigger that event. Our library is no different. Let's define a
`newEvent` function which will return a new `Event` as well as a function to trigger our `Event`.

```haskell
import Control.Monad.Trans -- for `liftIO`

import qualified Data.Map as M
import Data.Unique
import Data.IORef

newEvent :: React (Event a, a -> Moment ())
newEvent = do (registerListener, propagateListeners) <- newEventRegistration
              return (Event registerListener, propagateListeners)

newEventRegistration :: React (RegisterEventListener a, a -> Moment ())
newEventRegistration = do listeners <- newIORef M.empty
                          let registerListener listener =
                                do listenerKey <- newUnique
                                   modifyIORef listeners (M.insert listenerKey listener)
                                   return (modifyIORef listeners (M.delete listenerKey)
                              propagateListeners x =
                                do listeners' <- M.elems <$> liftIO (readIORef listeners)
                                   mapM_ ($ x) listeners'
                          return (registerListener, propagateListeners)
```

I won't go into all the details here, but basically, the sausage is made in the
`newEventRegistration` function which uses an `IORef` to keep track of registered listeners and a
`Unique` from the `Data.Unique` library to give each listener a unique key, which is useful for the
deregistration function.

### Hold your horses!

So now we can make an `Event` that we can fire by simply calling a function, but how do we make a
`Behavior`? Simple! Since `Behavior`'s are just steppers updated by events, we'll write a function
that will take an `Event a` and give us a `Behavior a`. However, because the `Event a` might not
fire immediately, we'll need to give the `Behavior a` an initial `a` to hold in the mean-time.

```haskell
import System.Mem.Weak

hold :: a -> Event a -> React (Behavior a)
hold initial updates =
  do cell <- newIORef initial
     let behavior = Behavior { _behaviorUpdates = () <$ updates
                             , _behaviorGetValue = liftIO (readIORef cell) }
     unregisterUpdates <- _eventRegisterListener updates (\x -> writeIORef cell x)
     addFinalizer behavior unregisterUpdates
     return behavior
```

Again, I won't go into too much detail here, but basically we create an `IORef` to store the current
value of the behavior, which we read out in the sampling function. The behavior updates event is
made by creating an `Event ()` from the `Event a` supplied to us (i.e., the behavior will update
whenever the event does). The `(<$)` function is given to us for free by virtue of `Event` being a
`Functor`. Finally, we register a listener for updates to the updating event, and update the cell
appropriately. We use the `addFinalizer` function from `System.Mem.Weak` to automatically unregister
our listener when `behavior` is garbage collected.

There is one more consideration that we're not taking into account here. Because we write to the
`IORef` immediately in the listener, it is possible that sampling the behavior at different points
in the `Moment` monad will give us different results. This is not what we want since a computation
in the moment monad should semantically run "at the same time." What we really want is for the cell
to be written after the current moment is complete. We can do this by updating our definition of the
`Moment` monad.

```haskell
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
import Control.Monad.Writer hiding (listen)

newtype Moment a = Moment { runMoment :: WriterT [IO ()] IO a }
    deriving (Monad, Applicative, Functor, MonadWriter [IO ()], MonadIO, MonadFix)

hold :: a -> Event a -> React (Behavior a)
hold initial updates =
  do cell <- newIORef initial
     ...
     unregisterUpdates <- _eventRegisterListener updates (\x -> tell [writeIORef cell x])
     ...
```

We will also need a function that can run a `Moment a` inside `IO`.

```haskell
import Control.Monad

sync :: Moment a -> IO a
sync m = do (a, updateHolds) <- runWriterT (runMoment m)
            sequence_ updateHolds
            return a
```

Note that we will also have to update our use of `Moment` elsewhere, including adding `liftIO`'s as
necessary. All of these changes are made in the [GitHub source](https://github.com/tathougies/react.hs).

## Wrapping things up in the real world

Finally, let's create some utility functions to listen to `Event`'s and `Behavior`'s, thus
completing our interactions with the real world. This will also allow us to keep our implementations
of `Event` and `Behavior` opaque.

```haskell
listen :: Event a -> (a -> Moment ()) -> IO (IO ())
listen = _eventRegisterListener

-- The listener here is called once to observe the behavior's initial value
-- and subsequently on updates. In some ways, it breaks the abstraction of
-- a continuous-time value, but it's a concession to practicality
listenToBehavior :: Behavior a -> (a -> Moment ()) -> IO (IO ())
listenToBehavior bb handle = do sync $ do initial <- sample bb
                                          handle initial
                                listen (_behaviorUpdates bb) (() -> sample bb >>= handle)

sample :: Behavior a -> Moment a
sample = _behaviorGetValue
```

Again, the functions here are pretty self-explanatory, except perhaps `listenToBehavior`. Basically,
we call the handler once with the initial sampled value of `bb` and then we register to updates from
`bb` and call the handler using the value of `bb.`

This all looks good, but there's a problem with sampling `bb` in the `_behaviorUpdates bb`
handler. Remember how we took care to make sure that `Behavior`'s do not update until the current
`Moment` is complete? This means that when we sample `bb` here we will be getting the *old*
value. That's not what we want! To remedy this, we'll use the same trick we used with `hold`. We
will modify `Moment` to keep track of IO actions to run after all the holds have been updated.

```haskell
newtype Moment a = Moment { runMoment :: WriterT ([IO ()], [IO ()]) IO a }
    deriving (Monad, Applicative, Functor, MonadWriter ([IO ()], [IO ()]), MonadIO, MonadFix)

listenToBehavior :: Behavior a -> (a -> Moment ()) -> IO (IO ())
listenToBehavior bb handle = do sync $ do initial <- sample bb
                                          handle initial
                                let handle' = sync (sample bb >>= handle)
                                listen (_behaviorUpdates bb) (() -> tell ([], [handle']))

sync :: Moment a -> IO a
sync m = do (a, (updateHolds, afterHolds)) <- runWriterT (runMoment m)
            sequence_ updateHolds
            sequence_ afterHolds
            return a
```

Again, previous `Moment` usages will have to be updated to reflect this new structure, which has
been done in the [GitHub source](https://github.com/tathougies/react.hs).

## Some other primitives

Modern FRP libraries usually contain a few other primitives, which we'll implement here. One of the
more important features is the ability to define accumulators, `Event`'s whose value depends on
previous values. Let's write the `accum` primitive now.

### Keeping state

```haskell
accum :: a -> Event (a -> a) -> React (Event a)
accum initial updaters =
  do cell <- newIORef
     (registerListener, propagateListeners) <- newEventRegistration
     let evt = Event registerListener
     unregisterEventListener <- _eventRegisterListener updaters $ \updater ->
                                do cellValue <- liftIO (modifyIORef cell updater >> readIORef cell)
                                   propagateListeners cellValue
     addFinalizer evt unregisterEventListener
     return evt
```

For `accum` we create an `IORef` to hold the current value, and then subscribe to the `updaters`
event directly. We must subscribe to `updaters` here, which means that `accum` must be run in the
`React` monad. If we didn't subscribe to `updaters` immediately, then it's possible that we will
miss some updates. We also play nice by registering a finalizer on the new event we create.

Using `accum` and `hold` we can make a `Behavior a` whose current value depends on previous ones as
well.

```haskell
accumB :: a -> Event (a -> a) -> React (Behavior a)
accumB initial updaters = do ea <- accum initial updaters
                             hold initial ea
```

### Spilling the beans

`reactive-banana` and `sodium` also have a primitive that takes an event whose value is a list and
returns an event of just values that fires for each element in the list. This is called something
like `spill.`

```haskell
spill :: Event [a] -> Event a
spill eas = Event (\listener ->
                   _eventRegisterListener eas $
                   \as -> mapM_ listener as)
```

Simply put, when someone listens to the `spill`ed event, we subscribe to the `Event [a]` and call
the listener for `Event a` on each element of the list.

### Calming down

`spill` gave us the ability to have one event produce multiple simultaneous events, but what if we
don't want to listen to all of those? What if we wanted to "un-`spill`"? Luckily, there's a
combinator for that, usually called `calm`. It takes an event that may fire more than once in a
given `Moment` and returns a new `Event` that will only fire once for the first firing of the
event. We will need to modify the `Moment` monad again to keep track of which events have already
been delivered.

```haskell
import Control.Monad.State

import qualified Data.Set as S

newtype Moment a = Moment { runMoment :: StateT (S.Set Unique) (WriterT ([IO ()], [IO ()]) IO) a }
  deriving (Monad, Applicative, Functor, MonadState (S.Set Unique), MonadWriter ([IO ()], [IO ()]),
           ,MonadIO, MonadFix)

sync :: Moment a -> IO a
sync m = do (a, (updateHolds, afterHolds)) <- runWriterT (evalStateT (runMoment m) S.empty)
            sequence_ updateHolds
            sequence_ afterHolds
            return a

calm :: Event a -> React (Event a)
calm evt = do key <- liftIO newUnique
              let evt = Event $ \listener -> _eventRegisterListener evt (calmed listener)
                  calmed listener a =
                    do alreadyCalled <- S.member key <$> get
                       when (not alreadyCalled) $ do
                         modify (S.insert key)
                         listener a
              return evt
```

Basically, `calm` adapts the listener to check if the listener has been called yet in this
moment. If it has not, then it calls the listener and notes in this `Moment` that the listener has
been called. Sweet!

### Switching things up

The most complicated combinator we're going to write today is `switchE` and `switch`. These
combinators let you dynamically change the state of the network. The `switchE` combinator takes a
`Behavior (Event a)` and returns an `Event a` that fires whenever the *current* `Event a` in the
given `Behavior` fires. This requires some trickery, but we actually don't need to write this in the
`React` monad.

```haskell
switchE :: Behavior (Event a) -> Event a
switchE be = Event $ \listener ->
             do eInitial <- sync $ sample be
                unregisterV <- newIORef (return ())
                unregisterListener <- _eventRegisterListener eInitial listener
                writeIORef unregisterV unregisterListener
                let switchToNewEvent =
                      do unregisterFromOld <- readIORef unregisterV
                         unregisterFromOld
                         eNext <- sync $ sample be
                         unregisterNewListener <- _eventRegisterListener eNext listener
                         writeIORef unregisterV unregisterNewListener
                unregisterBehaviorListener <- _eventRegisterListener (_behaviorUpdates be) $ \() ->
                                              tell ([], [switchToNewEvent])
                return $ do
                  unregisterEvtListener <- readIORef unregisterV
                  unregisterEvtListener
                  unregisterBehaviorListener
```

Basically, what this does is that, whenever a new listener subscribes, we subscribe to the current
`Event a` contained in the behavior, as well as the `Behavior` updates. The subscription to the
`Event a` will call our listener for events on the initial event until the `Behavior` updates. At
this point, the `switchToNewEvent` is called *after* all the new behavior values have been written
in the holds (that's why we use `tell` to register an `afterHolds` action). Because
`switchToNewEvent` is called after the current `Moment`, the listener will continue to be called on
firings of the old event until the end of the `Moment`. This is what we want, since our convention
is that `Behavior`'s do not update until the end of the current moment.

When `switchToNewEvent` is called, we read the current unregistration IO action from the `IORef` and
run it, thus disconnecting `listener` from future firings of the old `Event a`. We read the new
`Event a` from the behavior and register `listener` to listen to the new event instead. Finally, we
write out the deregistration function for the new event fo the `IORef`. The deregistration function
for the dynamically switched event disconnects both the listener from the current event and
`switchToNewEvent` from the behavior updates.

Whereas `switchE` dynamically switches between `Event a`'s, `switch` dynamically switches between
`Behavior`'s. Remember, we need to two things to define a behavior: an event that updates when the
behavior updates, and a way to read the current value. The `switchE` gives us an easy way to express
the former, and it's pretty straightforward to write a function to read the current value.

```haskell
switch :: Behavior (Behavior a) -> Behavior a
switch bb = Behavior { _behaviorUpdates = switchE (_behaviorUpdates <$> bb)
                     , _behaviorGetValue = do b <- sample bb
                                              sample b }
```

## Conclusion

And that's it! In this post, we've written a simple FRP implementation in less than 200
lines. Unlike it's more complicated cousins, like reactive-banana or sodium, this implementation
makes no performance or memory guarantees, and doesn't even have very strict semantics, but you'll
find it works pretty intuitively for *most things* (TM). In the next tutorial, we'll use our library
to write a simple game. Stay tuned for more, and be sure to check out the [GitHub source for
React.Hs](https://github.com/tathougies/react.hs).