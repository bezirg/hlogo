# HLogo: parallel NetLogo variant in Haskell

[![Build Status](https://travis-ci.org/bezirg/hlogo.svg)](https://travis-ci.org/bezirg/hlog) [![License (3-Clause BSD)](https://img.shields.io/badge/license-BSD--3-blue.svg?style=flat)](http://opensource.org/licenses/BSD-3-Clause)
 ([online API docs](http://bezirg.github.io/hlogo)) ([testing results](http://bezirg.github.io/hlogo/test-results.html))


## Prerequisites

Requires:

- [GHC](https://www.haskell.org/ghc/)>=7.6.1  and [cabal-install](http://hackage.haskell.org/package/cabal-install)>=1.18
- or [GHC platform](https://www.haskell.org/platform/)>=2013.2.0.0

Tested with GHC 7.6.3, 7.8.4, 7.10.2.

## Installing

```bash
cabal sandbox init  # optional but recommended for installing any needed dependencies only-locally
cabal install --enable-tests
```

This will automatically fetch any needed dependencies and install the HLogo library.

## Updating

```bash
git pull
cabal install --enable-tests
```

This will automatically fetch&update any needed dependencies and update the HLogo library.

## Example: Running an HLogo program

There are many HLogo examples under `bench/hlogo/` directory.
To compile an example, open a bash shell and run:

```bash
cabal exec ghc -- --make -O -XTemplateHaskell -XNoImplicitPrelude -cpp -threaded bench/hlogo/Termites.hs
```

To run the generated code:

```bash
./bench/hlogo/Termites --max-pxcor=10 --min-pxcor=-10 --max-pycor=10 --min-pycor=-10 +RTS -N2 # e.g. -N2 is for running on 2 cores
```

## Optional: Running tests

```bash
cabal test
```

## Optional: Running benchmarks

```bash
cabal bench
```

## Comparing with a NetLogo program

Assuming that the netlogo executables are in your `$PATH`, open a shell and run:

```bash
time netlogo-headless.sh --experiment experiment1 --model /PATH/TO/hlogo/bench/nlogo/Termites.nlogo 
```

