# Prerequisites

Requires:

- [GHC](https://www.haskell.org/ghc/)>=7.6.1  and [cabal-install](http://hackage.haskell.org/package/cabal-install)>=1.18
- or [GHC platform](https://www.haskell.org/platform/)>=2013.2.0.0

Tested with GHC 7.6.3, 7.8.4, 7.10.2.

# Installing

```bash
cabal sandbox init  # optional but recommended for installing any needed dependencies only-locally
cabal install --enable-tests
```

This will automatically fetch any needed dependencies and install the HLogo library.

# Updating

```bash
git pull
cabal install --enable-tests
```

This will automatically fetch&update any needed dependencies and update the HLogo library.

# Example: Running an HLogo program

There are many HLogo examples under `bench/src/hlogo/` directory.
To compile an example, open a bash shell and run:

```bash
cabal exec ghc -- --make -O -fth -threaded bench/src/hlogo/Simple1.hs
```

To run the generated code:

```bash
./bench/src/hlogo/Simple1 --max-pxcor=10 --min-pxcor=-10 --max-pycor=10 --min-pycor=-10 +RTS -N2 # e.g. -N2 is for running on 2 cores
```

# Optional: Running tests

```bash
cabal test
```

# Optional: Running benchmarks

```bash
cabal bench
```

# Comparing with a NetLogo program

Assuming that the netlogo executables are in your `$PATH`, open a shell and run:

```bash
time netlogo-headless.sh --experiment experiment1 --model /PATH/TO/hlogo/bench/src/netlogo/Simple1.nlogo 
```

