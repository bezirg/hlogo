# HLogo: parallel NetLogo variant in Haskell

[![Build Status](https://travis-ci.org/bezirg/hlogo.svg)](https://travis-ci.org/bezirg/hlogo) [![License (3-Clause BSD)](https://img.shields.io/badge/license-BSD--3-blue.svg?style=flat)](http://opensource.org/licenses/BSD-3-Clause)
 ([online API docs](http://bezirg.github.io/hlogo)) ([testing results](http://bezirg.github.io/hlogo/test-results.html))


## Software Requirements

- [GHC](https://www.haskell.org/ghc/)>=7.8.1  and [cabal-install](http://hackage.haskell.org/package/cabal-install)>=1.18
- or [GHC platform](https://www.haskell.org/platform/)>=2014.2.0.0

## Installing

Run inside your shell:

```bash
cabal sandbox init  # optional but recommended for installing any needed dependencies only-locally
cabal update
cabal install
```

This will automatically fetch any needed dependencies and install the HLogo library.

## Updating

```bash
git pull
cabal update
cabal install
```

This will automatically fetch&update any needed dependencies and update the HLogo library.

## Example: Running an HLogo program

There are many HLogo examples under `bench/hlogo/` directory.
To compile an HLogo model (e.g. RedBlue), inside the repository's directory open a shell and run:

```bash
cabal exec ghc -- --make -O -XTemplateHaskell -XNoImplicitPrelude -cpp -threaded bench/hlogo/RedBlue.hs
```

To run the generated code:

```bash
./bench/hlogo/RedBlue --max-pxcor=10 --min-pxcor=-10 --max-pycor=10 --min-pycor=-10 +RTS -N2 
# e.g. -N2 is for running on 2 cores
```

## Comparing with a NetLogo program

If you haven't add `NetLogo.jar` to your `CLASSPATH`, you have to locate its path in the filesystem. 
After that run:

```bash
java -Xmx1024m -cp PATH/TO/NetLogo.jar org.nlogo.headless.Main --model bench/nlogo/RedBlue.nlogo  
```

