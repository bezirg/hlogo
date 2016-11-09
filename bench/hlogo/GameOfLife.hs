{-# LANGUAGE TemplateHaskell, NoImplicitPrelude #-}

import Language.Logo

patches_own ["livingp", "live_neighbors"]


initial_density = 35
fgcolor = magenta - 2
bgcolor = turquoise + 4
true = 1
false = 0

args = ["--max-pxcor=50"
       ,"--max-pycor=50"
       ,"--min-pxcor=-50"
       ,"--min-pycor=-50"
       ,"--horizontal-wrap=True"
       ,"--vertical-wrap=True"
       ]
run ["setup", "go"]


setup = do
  ask (do
    r <- random_float 100.0
    if r < initial_density
    then cell_birth
    else cell_death) =<< patches
  reset_ticks

cell_birth = atomic $ do
  set_livingp true
  set_pcolor fgcolor

cell_death = atomic $ do
  set_livingp false
  set_pcolor bgcolor

go = forever $ do
  ts <- ticks
  when (ts > 300) $ stop
  ask (
        (atomic . set_live_neighbors . fromIntegral) =<< count =<< with (liftM (==true) livingp) =<< neighbors
      ) =<< patches
  -- Starting a new "ask patches" here ensures that all the patches
  -- finish executing the first ask before any of them start executing
  -- the second ask.  This keeps all the patches in synch with each other,
  -- so the births and deaths at each generation all happen in lockstep.
  ask (do
    lns <- live_neighbors
    if lns == 3
      then cell_birth
      else when (lns /= 2) cell_death) =<< patches
  tick

