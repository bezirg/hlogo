module Main where

import Framework.Logo.Keyword
import Framework.Logo.Prim
import Framework.Logo.Exception
import Framework.Logo.Base
import Control.Monad

globals ["initial_trees", "burned_trees"]
patches_own []
turtles_own []
breeds ["fires", "fire"]
breeds ["embers", "ember"]
breeds_own "fires" []
breeds_own "embers" []

setup = do
  --atomic $ ca
  ask_ (atomic $ set_pcolor green) =<< (with (liftM (< 99) (unsafe_random_float 100)) =<< unsafe_patches)
  ask_ ignite =<< (with (liftM2 (==) pxcor min_pxcor ) =<< unsafe_patches)
  sit <- count =<< (with (liftM (== green) unsafe_pcolor) =<< unsafe_patches)
  atomic $ set_initial_trees (fromIntegral sit)
  atomic $ set_burned_trees 0
  atomic $ reset_ticks

go = forever $ do
  ts <- unsafe_ticks
  when (ts > 200) (stop)
  ask_ (do
         ask_ ignite =<< (with (liftM (== green) unsafe_pcolor) =<< atomic neighbors4)
         atomic $ set_breed "embers"
       ) =<< unsafe_fires
  fade_embers
  atomic $ tick

ignite = do
  s <- atomic $ sprout_fires 1
  ask_ (atomic $ set_color red) s
  atomic $ set_pcolor black
  atomic $ set_burned_trees =<< liftM (+1) burned_trees

fade_embers = do
  ask_ (atomic $ do
          c <- liftM (\ x -> x - 0.3) color
          set_color c
          if c < red - 3.5 then set_pcolor c >> die else return ()) =<< unsafe_embers

run ['setup, 'go]
