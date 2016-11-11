{-# LANGUAGE TemplateHaskell, NoImplicitPrelude #-}
import Language.Logo

globals ["sum_of_spins"]
patches_own ["spin"]

temperature = 2.24

args = ["--max-pxcor=50"
       ,"--max-pycor=50"
       ,"--min-pxcor=-50"
       ,"--min-pycor=-50"
       ,"--vertical-wrap=True"
       ,"--horizontal-wrap=True"
       ]
run ["setup", "go"]

setup = do
  ask (do
         s <- one_of [-1,1]
         atomic $ set_spin s
         recolor
       ) =<< patches
  ss <- of_ spin =<< patches
  atomic $ set_sum_of_spins $ sum ss
  reset_ticks

go = forever $ do
  t <- ticks
  when (t > 50) $ stop
  ask update =<< n_of 2000 =<< patches
  tick

update = do
  s <- spin
  ns <- of_ spin =<< neighbors4
  let ediff = 2 * s * sum ns
  rf <- random_float 1.0
  when ((ediff <= 0) || (temperature > 0 && rf < exp ((- ediff) / temperature))) $ do
    atomic $ set_spin (-s)
    atomic $ with_sum_of_spins (+ (2*s))
    recolor

recolor = do
  s <- spin
  atomic $ set_pcolor $ if s == 1 then blue+2 else blue-2
  
       


