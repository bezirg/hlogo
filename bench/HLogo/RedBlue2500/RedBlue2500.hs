{-# LANGUAGE TemplateHaskell #-}
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
  ask_ (do
         [c] <- unsafe_one_of [black, black, black, black, black, black, black, black, red, blue]
         atomic $ set_pcolor c) =<< unsafe_patches
  atomic $ create_turtles 2500
  atomic $ reset_ticks

go = forever $ do
  t <- unsafe_ticks
  when (t==1000) $ stop
  ask_ (behave) =<< unsafe_turtles
  atomic $ tick

behave = do
  c <- unsafe_pcolor
  atomic $ fd 1 >> if c == red
                   then lt 30
                   else when (c == blue) (rt 30)

run ['setup, 'go]
