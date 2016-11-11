{-# LANGUAGE CPP, TemplateHaskell, NoImplicitPrelude #-}
import Language.Logo


#ifndef NR_TURTLES
#define NR_TURTLES 1000
#endif

args = ["--max-pxcor=50"
       ,"--max-pycor=50"
       ,"--min-pxcor=-50"
       ,"--min-pycor=-50"
       ,"--horizontal-wrap=True"
       ,"--vertical-wrap=True"
       ]

run ["setup", "go"]

setup = do
  create_turtles NR_TURTLES
  ask (do
         x <- random_xcor
         y <- random_ycor
         atomic $ setxy  x y 
      ) =<< turtles
  reset_ticks

go = forever $ do
  t <- ticks
  when (t==1000) stop
  ask (do move) =<< turtles
  tick

move = do
  r <- random 50
  l <- random 50
  atomic $ do
         rt r
         lt l
         fd 1

