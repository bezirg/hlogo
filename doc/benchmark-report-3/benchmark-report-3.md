% Report on HLogo (NetLogo clone in Haskell)
% Nikolaos Bezirgiannis, Ilias Sakellariou, Wishnu Prasetya
% 13/04/2013

# New benchmarks

I reran benchmarks varying the population of the turtles, because
it is apparent that HLogo is faster than NetLogo when the population is
low (not many conflicts happen), but the performance deteriorates when the population
increases (a lot of conflicts happen).


## Sheep

### Description

N sheep turtles move around a 100x100 torus eating grass (turning patches from green to brown). 
Grass regrows at a certain level (from brown to green). Stop after 1000 ticks.

### Results

population          netlogo-parsing     netlogo-jvm     hlogo1core      hlogo2cores4threads
----------          ---------------     -----------     ------          -------------------
100                 10                  8.2             7.4             5
250                 10.4                8.8             9.8             6.2
500                 11.4                9.7             13.3            7.7
1000                12.8                11.1            19.8            11.6
2000                14.3                12.6            31.1            16.8
3000                15.5                13.4            41.3            21.8             


### Graph

![Sheep benchmark results](sheep.eps)

## RedBlue

### Description

### Description

N turtles are moving forward 1 step on every tick. If they are on a red patch, they also
turn left by 30 degrees. If they are on a blue patch, they turn right by 30 degrees.
The benchmarking stop after 1000 ticks.


### Results

population          netlogo-parsing     netlogo-jvm     hlogo1core      hlogo2cores4threads
----------          ---------------     -----------     ------          -------------------
1000                2.1                 0.6             1.3             0.7
2500                3                   1.5             3.4             1.6
5000                4.9                 3.4             7.1             3.1
10000               8.9                 7.4             14.3            6.2
20000               17.5                16              28.4            12.3
30000               27.3                25.8            42.9            18.1             


### Graph


![RedBlue benchmark results](redblue.eps)



# New random number considerations

In HLogo, there are currently three ways to do random:

Old-safe
  : A single transactionally-safe variable holding a random number generator
Unsafe
  : A single transactionally-unsafe variable holding a random generator working in IO (may incur race conditions)
  : It was introduced to enable faster execution compared to old-safe
New-safe
  : 1-per agent transactionally-safe variable holding a distinct random number generator
  : It was introduced to enable faster execution compared to old-safe and similar performance compared to unsafe.

## Comparison of random

### Results

population          unsafe              new-safe        old-safe
---------           ------              --------        --------
100                 5.5                 5.2             6.2
250                 7                   6.6             8.7
500                 8.6                 8.6             12.8
1000                11.8                11.7            20
2000                17.8                17.4            34.4
3000                23.4                22.4            47.8

### Graph

![Random benchmark results](random.eps)


## Considerations

It looks that new-safe is as fast or even faster than unsafe. Unsafe and old-safe will become obsolete.

However there is an issue. Upon initiliazation, a distinct random seed has to be assigned to each agent (patch, turtle and link).

There is in Haskell the function `mkStdGen`, with type :

~~~
mkStdGen :: Int -> StdGen
~~~

which takes an Int and produces a distinct random generator.
The new-safe uses this method

- For turtle :  `mkStdGen who`
- For patch  :  `mkStdGen (pxcor + (pycor*1000))`
- For link   :  `mkStdGen (pxcor + (pycor*1000))`

However, random number seeds that are close are not strong and will yield similar results upon initiliazation. For example,

- For turtle 3: `mkStdGen 3`
- For turtle 4: `mkStdGen 4`

So, only upon initialization the initial random numbers yield are not `that` random, and a pattern evolves between them.

For example, upon initializing the sheep example, at tick 0:

![Sheep initialization at tick 0](sheep0.eps)


### Question

In your opinion how the initial seeds per agent should be computed?