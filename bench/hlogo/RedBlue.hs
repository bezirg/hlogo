{-# LANGUAGE CPP, TemplateHaskell, NoImplicitPrelude #-}
import Language.Logo

#ifndef NR_TURTLES
#define NR_TURTLES 10
#endif

args = ["--max-pxcor=100"
       ,"--max-pycor=100"
       ,"--min-pxcor=-100"
       ,"--min-pycor=-100"
       ,"--horizontal-wrap=True"
       ,"--vertical-wrap=True"
       ]

run ["setup", "go"]

setup = do
  ask (do
         c <- one_of [black, black, black, black, black, black, black, black, red, blue]
         atomic $ set_pcolor c) =<< patches
  create_turtles NR_TURTLES
  ask (do
         x <- random_xcor
         y <- random_ycor
         atomic $ setxy x y
      ) =<< turtles
  reset_ticks

go = forever $ do
  t <- ticks
  when (t==1000) stop
  ask behave =<< turtles
  tick

behave = do
  c <- pcolor
  atomic $ fd 1
  if c == red
   then atomic $ lt 30
   else when (c == blue) (atomic $ rt 30)




