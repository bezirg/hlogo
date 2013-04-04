module Main where

import Framework.Logo.Keyword
import Framework.Logo.Prim
import Framework.Logo.Exception
import Framework.Logo.Base
import Control.Monad

globals []
patches_own []
turtles_own []

setup = do
  -- ask_ (do
  --        atomic (set_color white)
  --        x <- unsafe_random_xcor
  --        y <- unsafe_random_ycor
  --        atomic $ do
  --          setxy x y
  --          set_size 5) ts
  atomic $ create_turtles 100000
  atomic $ reset_ticks

go = forever $ do
  t <- unsafe_ticks
  ask_ (behave >> behave) =<< unsafe_turtles
  stop

behave = do
  atomic (forward 1 >> forward 1)
  atomic (back 1 >> forward 1)

run ['setup, 'go]
