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
  atomic $ create_turtles 100000
  atomic $ reset_ticks

go = forever $ do
  t <- unsafe_ticks
  when (t==8) $ stop
  ask_ (behave) =<< unsafe_turtles
  unsafe_show_ t
  atomic $ tick

behave = do
   atomic $ do 
           (forward 1 >> forward 1)
           (back 1 >> forward 1)

   atomic $ do 
           (forward 1 >> forward 1)
           (back 1 >> forward 1)

   atomic $ do 
           (forward 1 >> forward 1)
           (back 1 >> forward 1)

   atomic $ do 
           (forward 1 >> forward 1)
           (back 1 >> forward 1)


run ['setup, 'go]
