---
title: Making Money With Free Monads (Part 1)
author: Travis Athougies
tags: "finance, haskell"
published: true
---

I've recently begun investigating using Haskell in the financial sphere to do investment analysis
and eventually quant trading. I've started by creating some APIs (like this
[TradeKing API for Haskell](/projects/tradeking.html) and this
[interface to SEC Edgar](/projects/eddy.html)). But now I'm turning my eyes towards writing full
quant trading models with Haskell.

However, I want to do this in the _most_ beautiful way possible. What does this mean, you ask? It
means I want my models to take full advantage of Haskell's static type checking abilities. A lot of
errors in other programming languages are, when you really get down to it, type errors, even when
they don't obviously seem so. In Haskell it is common to roll your own types on the fly, to make
them as specific as possible for your use case. Not only does this let you prove things about your
code, but it also means you won't accidentally pass in a stock price, when you actually needed
earnings per share. For example, you would use a ``Price`` type for the stock price and a
``PricePerShare`` type for the EPS, and then use a function that would take a ``PricePerShare``
object and the number of shares and return the total capitalization as a ``Price`` object, for
example.

Haskell also has a few other nice properties that make it suitable for quant-trading: it is
compiled, so it has speed comparable with C/C++. It is probably the language _most_ suited for
widely parallel problems which involve a lot of I/O (i.e., monitoring thousands of different
securities). However, probably the most killer feature of them all is the ability to write ad-hoc
DSLs and embed the in the language by using Monads.

In my Haskell quant toolbox, I want to be able to write a certain trading strategy composed of
certain actions, such as "check stock price" and "sell stock A". Additionally, I want to be able to
backtest this strategy on historical data, to run it live in production, and to run it in "paper
trading" mode.

One way to do this would be to create a new language for quant strategies and then implement a
parser to make an AST and then use some kind of interpreter to run the whole thing. This is a
solution, but it seems awfully inefficient. I don't want to be spending my time writing parsers
while there are arbitrage opportunities to exploit!

Luckily for us, Haskell makes it really easy to embed languages write into our source code, using a
construct called _Free Monads_.  I won't go into all the theory (which you can read about
[here]()), but basically this allows us to build abstract syntax trees of an arbitrary type _inside_
a Haskell source file. We the write interpreters for these trees which implement the actual
actions. Since these ASTs are implemented in the form of Haskell Monads, we get all the nice
functions that generalize to any monad for free (for example, for and while loops!).

Defining Types
-------------

Enough talk; let's build something! First, we're going to define our types. We're going to make one
type ``StategyPart`` which will correspond to all the different things we want our strategies to be
able to do. For now, we want our strategy to be able to look at the current price of stocks and to
buy or sell these securities.

To begin, we're going to define a type for stock symbols ``Symbol``, a type for number of shares,
and also a fixed-point (to 3 decimal places)``Price`` type (using the ``Data.Fixed`` package).

``` haskell
{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings #-}
import Data.Text
import Data.Fixed
import Data.String

newtype Symbol = Symbol { unSymbol :: Text }
    deriving (Show, Read, Eq, Ord, IsString)
newtype Price  = Price  { unPrice  :: Fixed E3 }
    deriving (Show, Read, Eq, Ord, Num)
newtype Shares = Shares { unShares :: Int }
    deriving (Show, Read, Eq, Ord, Num, Real, Bounded, Enum)
```

Notice that we enable the ``GenericNewtypeDeriving`` extension. This lets us define ``newtype``s
that basically act as though they were members of the underlying type. This gets us type safety and
convenience!

With these in mind, we can define our ``StrategyPart`` type.

``` haskell
data StrategyPart next = QuoteSymbol Symbol (Price -> next) |
                         Buy Symbol Shares  next |
                         Sell Symbol Shares next
```

This type seems self explanatory, except for the last parameter to each constructor. What is that
funny ``next`` parameter, you might ask? Notice how each of the constructors of ``StrategyPart``
represents a certain action to perform. The last parameter tells us what to do after these actions
complete, and what to do with the result. The idea is that this ``QuoteSymbol`` constructor will hold
a symbol that we want to quote as well as a function for performing on the result of that quote.

But wait! Recall, we said we were going to be creating a ``Monad`` that built an AST. Right now we
have a type to represent an AST, but ``StrategyPart`` isn't a ``Monad``! This is where the magic of
free monads come to the rescue. Using the ``Free`` monad from the ``free`` package, we can get a
``Monad`` instance from StrategyPart for free! This is quite literally from where the name "Free monad"
comes: once you define the type, the Monad can be derived for free!

``` haskell
import Control.Monad.Free

-- | A strategy that returns a value of type a
type Strategy a = Free StrategyPart a
```

Well, actually, it turns out that we don't exactly get a ``Monad`` for free. In order for a ``Monad``
instance to be derived, we have to make ``StrategyPart`` a ``Functor``. Don't feel slighted though:
it turns out that this is the absolute minimum you must specify in order to get a ``Monad`` (or so
the category theorists tell us). This is easy to do though:

``` haskell
instance Functor StrategyPart where
    fmap f (QuoteSymbol sym g) = QuoteSymbol sym (f . g)
    fmap f (Buy sym shares g) = Buy sym shares (f g)
    fmap f (Sell sym shares g) = Sell sym shares (f g)
```

Hmm. Well that was do-able, but it's not really _easy_. Luckily, we don't even have to write out
this instance! GHC is smart enough to derive it for us. All we need to is enable the
``DeriveFunctor`` extension.

``` haskell
{-# LANGUAGE GenericNewtypeDeriving, DeriveFunctor #-}

...

data StrategyPart next = QuoteSymbol Symbol (Price -> next) |
                         Buy Symbol Shares  next |
                         Sell Symbol Shares next
    deriving Functor
```

That's it! We're ready to make a build a real strategy.

For our first strategy we're going to be really stupid and blindly purchase 10 shares of Netflix
(NFLX), and then sell them immediately.

``` haskell
-- | Let's hope the random price fluctuations are working in our favor!
myStupidStrategy :: Strategy ()
myStupidStrategy = do
    buy (Symbol "NFLX") (Shares 10)
    sell (Symbol "NFLX") (Shares 10)
```

If we try to compile this, we'll probably get an error about how ``buy`` and ``sell`` don't exist,
which makes sense, since we haven't written them yet! Turns out that even though we have the ``Buy``
and ``Sell`` constructors, we can't use them directly. They need to be _lifted_ into the
monad. Luckily the ``Control.Monad.Free`` package has our backs.

``` haskell
buy, sell :: Symbol -> Shares -> Strategy ()
buy sym shares = liftF (Buy sym shares ())
sell sym shares = liftF (Sell sym shares ())
```

But wait! We have a ``Strategy`` but how do we run it? After all, it's not linked to the IO monad,
and presumably ``Strategy``s should be able to interface with the real world? This is where the
beauty of free monads come in. The free monad lets us write our own interpreters, accepting a value
of type ``Strategy`` and then evaluating them in whatever way they want. Because this is a bad
trading strategy, we wouldn't want to run it in the real world. Luckily we can still "run" this
strategy in a simple interpreter that simply reports the buying and/or selling decisions of the
strategy.

``` haskell
logTrades :: Strategy a -> [(Symbol, Shares)]
logTrades strategy = doLog strategy id
    where doLog strategy a = do
              case strategy of
                  Free (Buy sym shares next) -> doLog next (((sym, shares):) . a)
                  Free (Sell sym shares next) -> doLog next (((sym, negate shares):) . a)
                  Free  _ -> error "Unsupported operation run in trade logging interpreter"
                  Pure _ -> a []
```

There is quite a bit going on here, so let's take it one bit at a time. The first line

``` haskell
logTrades :: Strategy a -> [(Symbol, Shares)]
```

says that we have a function ``logTrades`` which takes a strategy producing any value and produces a
list of symbol and share pairs. The next two lines

``` haskell
logTrades strategy = doLog strategy id
    where doLog strategy a = do
```

are where we set ourselves up for the tail-recursive function ``doLog``. This function will thread
the trade log state through our strategy. The accumulator will be a function of type
``[(Symbol, Shares)] -> [(Symbol, Shares)]``. The trick we use to pass the accumulator is a common
Haskell idiom for building up a list in order (if we used the cons operator ``(:)`` by itself, we 'd
end up building the list backwards). Finally, the next lines implement the actual interpreter.

``` haskell
              case strategy of
                  Free (Buy sym shares next) -> doLog next (((sym, shares):) . a)
                  Free (Sell sym shares next) -> doLog next (((sym, negate shares):) . a)
```

These two lines handle the buy and sell commands by adding them to the end of the list. The buy
command adds a symbol-share pair with a positive share value, while the sell command adds one with a
negative value.

The next line

``` haskell
                  Free  _ -> error "Unsupported operation run in trade logging interpreter"
```

throws an error if any command other than buy or sell, like a stock quote, is run in the strategy.

Finally, the last line handles the base case, when our strategy finishes executing.

``` haskell
                  Pure _ -> a []
```

Now, if we run this interpreter on our strategy we should get something like the following

``` haskell
Prelude> logTrades myStupidStrategy
[(Stock {unStock="NFLX"}, Shares {unShares=10}), (Stock {nStock="NFLX"}, Shares {unShares=-10})]
```

That's it for this post. Check out part 2 (coming soon) for more!.