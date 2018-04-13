---
title: "The monadic structure of combinatorial optimization"
author: "Travis Athougies"
tags: "haskell,math"
published: true
---

Combinatorial optimization refers to assigning discrete values to a set of
variables with the aim to minimize (or equivalently, maximize) a given objective
function.

Monads offer us a way to abstract over the notion of variable assignment via the
bind (`>>=`) operator, and thus form a natural way to express an optimization
problem in Haskell.

This file is Literate Haskell, so we'll need some imports and one extension.

> {-# LANGUAGE DeriveFunctor #-}
> {-# LANGUAGE FlexibleContexts #-}
> import Control.Monad (ap)
> import Control.Monad.Trans (lift)
> import Control.Monad.Writer (WriterT, runWriterT, tell)
> import Data.Ord (comparing)
> import Data.Function (on)
> import System.Random

One straightforward way to solve optimization problems is considering every
possible solution, and choosing the one that minimizes the objective
function. For example, Suppose we want to solve the very simple optimization
problem

\\[
\\begin{aligned}
  & \\underset{x, y}{\\text{minimize}}
  & & x + y \\\\
  & \\text{subject to}
  & & x \\in \\{ 1, 2 \\}, y \\in \\{ 3, 4 \\}
\\end{aligned}
\\]

Then, we can express all possible solutions to this problem using lists

> simpleOpt :: [Double]
> simpleOpt =
>  do x <- [1, 2]
>     y <- [3, 4]
>     pure (x + y)

Now, we can find the minimum using the `minimum` function

```console
Main*> minimum simpleOpt
4.0
```

`simpleOpt` was quite straightforward. Other optimization problems are
more demanding. The [the traveling salesman
problem](http://katrinaeg.com/simulated-annealing.html) asks us to
find the length of the shortest route through a given set of cities,
visiting each city only once, and returning to the original
city. Here, our next choice of city depends on which cities we've
already written.

We can use the list monad to formulate every possible solution to an arbitrary
traveling salesman problem.

> type City = Int -- use Int to represent a city, for now
>
> tsp :: [ City ] -> (City -> City -> Double)
>     -> [ Double ]
> tsp [] _ = pure 0
> tsp (firstCity:cities) distance = go 0 firstCity cities
>   where
>     go curDistance lastCity [] = pure (distance firstCity lastCity + curDistance)
>     go curDistance lastCity unvisited =
>       do nextCity <- unvisited
>
>          let unvisited' = filter (/=nextCity) unvisited
>          go (curDistance + distance nextCity lastCity) nextCity unvisited'


Solving the Traveling Salesman Problem
---

Let's define an instance of the traveling salesman problem, with 15 cities. We'll represent
distances between the cites using a 15x15 distance matrix, encoded as nested
lists (this example is meant to illustrate, not perform!).

> distMatrix :: [ [ Double ] ]
> distMatrix =
>   [ [  0, 29, 82, 46, 68, 52, 72, 42, 51, 55, 29, 74, 23, 72, 46 ]
>   , [ 29,  0, 55, 46, 42, 43, 43, 23, 23, 31, 41, 51, 11, 52, 21 ]
>   , [ 82, 55,  0, 68, 46, 55, 23, 43, 41, 29, 79, 21, 64, 31, 51 ]
>   , [ 46, 46, 68,  0, 82, 15, 72, 31, 62, 42, 21, 51, 51, 43, 64 ]
>   , [ 68, 42, 46, 82,  0, 74, 23, 52, 21, 46, 82, 58, 46, 65, 23 ]
>   , [ 52, 43, 55, 15, 74,  0, 61, 23, 55, 31, 33, 37, 51, 29, 59 ]
>   , [ 72, 43, 23, 72, 23, 61,  0, 42, 23, 31, 77, 37, 51, 46, 33 ]
>   , [ 42, 23, 43, 31, 52, 23, 42,  0, 33, 15, 37, 33, 33, 31, 37 ]
>   , [ 51, 23, 41, 62, 21, 55, 23, 33,  0, 29, 62, 46, 29, 51, 11 ]
>   , [ 55, 31, 29, 42, 46, 31, 31, 15, 29,  0, 51, 21, 41, 23, 37 ]
>   , [ 29, 41, 79, 21, 82, 33, 77, 37, 62, 51,  0, 65, 42, 59, 61 ]
>   , [ 74, 51, 21, 51, 58, 37, 37, 33, 46, 21, 65,  0, 61, 11, 55 ]
>   , [ 23, 11, 64, 51, 46, 51, 51, 33, 29, 41, 42, 61,  0, 62, 23 ]
>   , [ 72, 52, 31, 43, 65, 29, 46, 31, 51, 23, 59, 11, 62,  0, 59 ]
>   , [ 46, 21, 51, 64, 23, 59, 33, 37, 11, 37, 61, 55, 23, 59,  0 ]
>   ]
>
> distFunc :: City -> City -> Double
> distFunc a b = (distMatrix !! a) !! b
>
> allCities :: [ City ]
> allCities = [ 0 .. 14 ]


Solving `simpleOpt` was easy. Let's see what happens if we attempt to minimize
the value of `tsp` using the parameters above.

```console
*Main> minimum (tsp allCities distFunc)
... Wait until end of universe ...
Profit!
```

As you can see, not much happens. Under the hood, Haskell is busy calculating
every single path, trying to find one that works. Sadly, there are trillions of
paths, so this search will take a very long time. By the time we have 60 cities
on our itinerary, we would have to examine more paths than there are particles
in the known universe. Clearly, we need a better strategy.

If you can't solve a problem, guess
--

When problems like the traveling salesman problem come up in practice,
oftentimes, we don't need the optimal solution, and can move forward
with any solution that's good enough.

[Simulated annealing](https://en.wikipedia.org/wiki/Simulated_annealing) is one
such strategy to find a good enough solution. It is inspired by the
metallurgical process of *annealing*, whereby metal atoms are heated to high
temperatures and then slowly cooled to increase the overall strength of the
object.

The basic simulated annealing algorithm is as follows:

* Choose an initial solution at random, and set a high temperature *T*.
* Repeat the following steps until the system has "cooled":
  * Select a random perturbation of the current solution
  * If the perturbation is better than the current solution, move
    towards it.  Otherwise, randomly decide to move to the neighbor,
    with a probability proportional to *T*.

    For the purposes of this article, we'll accept a worse solution with probability

    \\[P(\\text{choose worse}) = \\exp\\left(-\\frac{1}{T}\\left(\\frac{f(neighbor) - f(current)}{f(current)}\\right)\\right)\\]

    where *f* is the objective function.

  * Lower *T* systematically.
* Return the best seen solution

Laziness for the win
---

When we think of any combinatorial optimization problem, we soon realize that
any solution can be thought of as a path from the root of a tree to any leaf.

For example, for `simpleOpt`, we can construct a tree representing choices for
`x` and `y`.

![The tree for `simpleOpt`](image:dot/comb-opt/simpleopt.png)

The traveling salesman tree is a lot larger, but is straightforwards to
construct.

![The tree for `exampleTsp`](image:dot/comb-opt/tsp.png)

Note that the interior values associated with any particular choice is
immaterial. All that matters is the structure of the edges. In the diagrams above, we notated each edge with the assignment it represents. The leaves are notated with solutions.

We can trivially encode such trees with a Haskell data type.

> data Combinatorial a
>    = Choice Int (Combinatorial a) [Combinatorial a]
>    | Leaf a

Here, `Choice` represents a node with children. It contains one privileged child
(the next in our path) along with a list of children not in our path. The `Int`
argument is simply the cached degrees of freedom of the next child (more on this
later). As expected `Leaf` is simply a node with no children, that represents
the final result of a choice.

`Combinatorial` is trivially a `Functor`, and we can just ask GHC to figure that
one out for us.

>    deriving (Show, Functor)


Let's define a function to figure out the number of degrees of freedom in a
given `Combinatorial`. This helps in choosing a random neighbor.

Firstly, a single value has no degrees of freedom

> degreesOfFreedom :: Combinatorial a -> Int
> degreesOfFreedom Leaf {} = 0

A `Choice` has the degrees of freedom of its current child plus the
possibilities of choosing one of its other subtrees.

> degreesOfFreedom (Choice childSz _ remaining) = length remaining + childSz

We can also write a function to get the solution at the chosen path.

> currentSolution :: Combinatorial a -> a
> currentSolution (Leaf a) = a
> currentSolution (Choice _ next _) = currentSolution next

A bit of thinking, and there are natural applicative and monadic interfaces.

> instance Applicative Combinatorial where
>   pure = Leaf
>   (<*>) = ap
>
> instance Monad Combinatorial where
>   return = pure
>   Leaf a >>= b = b a
>   Choice sz next rest >>= f =
>     let next' = next >>= f
>         sz'   = degreesOfFreedom next'
>     in Choice sz' next' (map (>>= f) rest)


We can write a function to introduce non-determinism into
`Combinatorial`. `choose` takes a non-empty list of possible values (the
*domain*) of a variable; it yields the current choice. For the sake of
simplicity, we `error` on an empty list, but a more robust solution would use
`Data.List.NonEmpty` or another solution.

> choose :: [a] -> Combinatorial a
> choose [] = error "Empty domain"
> choose [a] = Leaf a
> choose (a:as) = Choice 0 (Leaf a) (map Leaf as)

Now, we can encode `tsp` in `Combinatorial`.

> tspCombinatorial :: [ City ] -> (City -> City -> Double)
>                  -> Combinatorial Double
> tspCombinatorial [] _ = pure 0
> tspCombinatorial (firstCity:cities) distance = go 0 firstCity cities
>   where
>     go curDistance lastCity [] = pure (distance firstCity lastCity + curDistance)
>     go curDistance lastCity unvisited =
>       do nextCity <- choose unvisited
>
>          let unvisited' = filter (/=nextCity) unvisited
>          go (curDistance + distance nextCity lastCity) nextCity unvisited'

Now, for the problem above, we can form all possible combinations.

> exampleTsp :: Combinatorial Double
> exampleTsp = tspCombinatorial allCities distFunc

Now, you may think that if we try to inspect `exampleTsp`, we'll be in for a
world of hurt. However, due to Haskell's laziness, we can freely ask GHC for the
value of `tsp` at the chosen path, without evaluating any other possible
path. This is thanks to Haskell's laziness -- we can inspect partial solutions
of the TSP problem without demainding the whole thing. Best of all, once we
inspect the initial path, we never have to compute it again -- the value of
`tsp` at this path is cached, so long as we access it through `exampleTsp`.

Hitting the metal
---

So we now have a data structure in which to express a set of solutions along
with a chosen solution and a strategy to search this set for a 'good enough'
solution. The only things we still need are functions to pick a random
'neighbor' solution and a function to randomize a solution.

First, we'll write a function to pick a random neighbor. We'll consider a
solution a neighbor of the current one if it chooses a different subtree at
exactly *one* point in the current path. We use `degreesOfFreedom` to figure out
where to choose a different path.

We'll use `RandomGen` from `System.Random` to abstract over our random generator.

> pickNeighbor :: RandomGen g => Combinatorial a -> g -> (Combinatorial a, g)
> pickNeighbor c g =
>   let dof = degreesOfFreedom c
>       (forkAt, g') = randomR (0, dof - 1) g
>   in (neighborAt forkAt c, g')
>
> neighborAt :: Int -> Combinatorial a -> Combinatorial a
> neighborAt _ c@(Leaf {}) = c
> neighborAt i (Choice sz next rest)
>   | i < sz = let next' = neighborAt i next
>              in Choice (degreesOfFreedom next') next' rest
>   | otherwise =
>       let restIdx = i - sz
>           (restBefore, next':restAfter) = splitAt restIdx rest
>
>           rest' = next:(restBefore ++ restAfter)
>       in Choice (degreesOfFreedom next') next' rest'

Now, to randomize a given solution, we'll just choose a random neighbor an
arbitrary number of times.

> randomize :: RandomGen g => Combinatorial a -> g -> (Combinatorial a, g)
> randomize c g =
>   let (numRandomizations, g') = randomR (0, degreesOfFreedom c) g
>
>       randomizeNTimes 0 c g'' = (c, g'')
>       randomizeNTimes n c g'' =
>         let (c', g''') = pickNeighbor c g''
>         in randomizeNTimes (n - 1) c' g'''
>   in randomizeNTimes numRandomizations c g'

We are all set!

We can now define an `anneal` function that, given a `Combinatorial` and
parameters for the annealing process, chooses a best guess. We'll need to
provide a random generator as well

> type Temperature = Double
> anneal :: ( RandomGen g )
>        => ( energy -> energy -> Ordering)
>        -> ( energy -> energy -> Temperature -> Double) -- ^ Probability of accepting a worse solution
>        -> Temperature -- ^ Starting temperature
>        -> Temperature -- ^ Ending temperature
>        -> Double -- ^ Cooling factor
>        -> Combinatorial energy -> g -> (Combinatorial energy, g)
> anneal cmp acceptanceProbability tInitial tFinal coolingFactor initialSolution g =
>   let (initial', g') = randomize initialSolution g
>
>       doAnneal curTemp current best curRandom
>         | curTemp <= tFinal = (best, curRandom)
>         | otherwise =
>             let (neighbor, curRandom') = pickNeighbor current curRandom
>
>                 currentValue = currentSolution current
>                 neighborValue = currentSolution neighbor
>
>                 (diceRoll, curRandom'') = randomR (0, 1) curRandom'
>                 acceptNeighbor = cmp neighborValue currentValue == LT ||
>                                  acceptanceProbability currentValue neighborValue curTemp > diceRoll
>
>                 (current', current'Value) =
>                   if acceptNeighbor
>                   then (neighbor, neighborValue)
>                   else (current, currentValue)
>
>                 best' = if cmp current'Value (currentSolution best) == LT
>                         then current'
>                         else best
>             in doAnneal (curTemp * (1 - coolingFactor)) current' best' curRandom''
>   in doAnneal tInitial initial' initial' g'
>
> defaultAcceptanceProbability :: Double -> Double -> Temperature -> Double
> defaultAcceptanceProbability curValue neighborValue curTemp =
>   exp (-(neighborValue - curValue)/(curValue * curTemp))

Now we can solve our traveling salesman problem stochastically. First, a utility
function to run the annealing in the `IO` monad using a new random generator.

> runAnneal :: Temperature -> IO Double
> runAnneal tInitial = do
>   g <- newStdGen
>   let (result, _) = anneal compare defaultAcceptanceProbability tInitial 1 0.003 exampleTsp g
>   pure (currentSolution result)

Running it gives (results will obviously vary due to randomness):

```haskell
*Main> runAnneal 100000
488.0
```

This is certainly better than the naive solution of visiting every city in order:

```haskell
*Main> head (tsp allCities distFunc)
771.0
```

Asking for directions
---

Because `Combinatorial` is a `Monad`, we can use all the normal
`Monad` tricks. For example, if we want to get the path along with the
length, we can use `MonadWriter`.

> tspWithPath :: [ City ] -> (City -> City -> Double)
>             -> WriterT [City] Combinatorial Double
> tspWithPath [] _ = pure 0
> tspWithPath (firstCity:cities) distance = go 0 firstCity cities
>   where
>     go curDistance lastCity [] = pure (distance firstCity lastCity + curDistance)
>     go curDistance lastCity unvisited =
>       do nextCity <- lift (choose unvisited)
>          tell [nextCity]
>
>          let unvisited' = filter (/=nextCity) unvisited
>          go (curDistance + distance nextCity lastCity) nextCity unvisited'
>
> exampleTspWithPath :: Combinatorial (Double, [City])
> exampleTspWithPath = runWriterT (tspWithPath allCities distFunc)
>
> runAnnealWithPath :: Temperature -> IO (Double, [City])
> runAnnealWithPath tInitial = do
>   g <- newStdGen
>   let (result, _) = anneal (comparing fst) (defaultAcceptanceProbability `on` fst)
>                            tInitial 1 0.003 exampleTspWithPath g
>   pure (currentSolution result)

Running this we get

```console
*Main> runAnnealWithPath 100000
(465.0,[10,5,3,12,1,2,6,8,4,7,9,11,13,14])
```

Discussion
---

Above, we saw how we could give monadic structure to an algebraic tree with a
privileged path. We used this primitive as a structure on which to run simulated
annealing problems. We illustrated how to encode a common NP-complete problem in
this way.

Our annealing function is not limited to traveling salesman problems. We can
encode any NP-complete problem where we can 'grade' any solution such that the
optimal solution has the lowest grade. This is not just of theoretical
significance. NP complete problems arise every day. For example, database
engines face the issue of join ordering for optimal query performance. Our
little framework provides a way to describe these problems monadically, and then
to evaluate them fully when the search space is small or use an appropriate
search method when they become intractable. This is similar to how Postgres
optimizes joins.

Because our tree structure comes with monadic operations, we don't need to worry
about coming up with specific representations of these problems, as libraries in
other languages require. For example, the equivalent problem encoded in C using
a [standard simulated annealing
library](https://www.gnu.org/software/gsl/doc/html/siman.html) takes up almost
70 lines of code. In contrast, ours took only eleven, and could likely be
simplified even more.

There are a few problems with our solution. First of all, we haven't proven any
kind of convergence to an optimal solution. In particular, I worry if the way in
which we choose neighbors (change our choice of branching at one node)
introduces any kind of bias in our system, and whether this affects quality of
our solutions.

Our solution used a data structure that keeps around solutions which we've
visited, but are known to be non-optimal. We could be smarter in allowing these
solutions to be garbage collected and marked as inaccessible (a form of tabu
search), or change our representation entirely to avoid this. It remains to be
seen if this is useful.

Future work
---

Our tree-like structure for representing combinations can be thought of as
containing the entire set of (as-yet uncomputed) solutions. In this article, we
used simulated annealing to search through this structure for an optimal
solution. However, there are multiple other strategies (termed
*meta-heuristics*) we could use. It's an interesting exercise for the reader to
implement search strategies, such as genetic algorithms, particle swarm
optimization, ant colony optimization, or others. The possibilities are as
endless as the possible paths through our fifteen cities!

Another interesting exploration would be to figure out what kinds of problems
could be encoded using monads versus applicative. Above, we saw that the
traveling salesman problem (which was NP-complete) requires the monadic bind. We
expressed `simpleOpt` using monadic operations, but it could easily have been
written using applicative. Part of me feels like all the polynomial time
problems can be expressed with applicative, and we'd need monad for the NP
complete ones, but I haven't thought too much on it. There are other classes of
optimization problems that could be interesting. For example, there are some
problems that can be solved via linear programming. I wonder if we can use our
data type to solve those.

These are thoughts for another day.
