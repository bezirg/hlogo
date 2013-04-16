{-# LANGUAGE TemplateHaskell #-}
module Main where

import Framework.Logo.Keyword
import Framework.Logo.Prim
import Framework.Logo.Exception
import Framework.Logo.Base
import Control.Monad

globals []
patches_own ["countdown"]
breeds ["wolves", "wolf"]
breeds ["sheep", "a_sheep"]
breeds_own "wolves" ["wenergy"]
breeds_own "sheep" ["senergy"]

-- Model Parameters
grassp = True
grass_regrowth_time = 30
initial_number_sheep = 100
initial_number_wolves = 0
sheep_gain_from_food = 4
wolf_gain_from_food = 20
sheep_reproduce = 4
wolf_reproduce = 5

setup = do
  ask_ (atomic $ set_pcolor green) =<< unsafe_patches
  when grassp $ ask_ (do
                       r <- unsafe_random grass_regrowth_time
                       c <- liftM head (unsafe_one_of [green, brown])
                       atomic $ do
                         set_countdown r
                         set_pcolor c
                     ) =<< unsafe_patches

  s <- atomic $ create_sheep initial_number_sheep
  ask_ (do
          s <- unsafe_random (2 * sheep_gain_from_food)
          x <- unsafe_random_xcor
          y <- unsafe_random_ycor
          atomic $ do
            set_color white
            set_size 1.5
            set_label_color (blue -2)
            set_senergy s
            setxy x y
       ) s
  atomic $ reset_ticks


go = forever $ do
  t <- unsafe_ticks
  when (t > 1000) (unsafe_sheep >>= count >>= unsafe_print_ >> stop)
  ask_ (do
         move
         e <- unsafe_senergy
         when grassp $ do
            atomic $ set_senergy (e -1)
            eat_grass
         if (e-1 < 0 && grassp) then (atomic die) else reproduce_sheep
       ) =<< unsafe_sheep
  when grassp (ask_ grow_grass =<< unsafe_patches)
  atomic $ tick

move = do
  r <- unsafe_random 50
  l <- unsafe_random 50
  atomic $ do
         rt r
         lt l
         fd 1

eat_grass = do
  c <- unsafe_pcolor
  when (c == green) $ do
              atomic $ set_pcolor brown
              atomic $ with_senergy (+ sheep_gain_from_food)

reproduce_sheep = do
  r <- unsafe_random_float 100
  when (r < sheep_reproduce) $ do
                    w <- atomic $ with_senergy (/ 2) >> hatch 1
                    ask_ ((unsafe_random_float 360 >>= \ r -> atomic (rt r >> fd 1))) w

grow_grass = do
  c <- unsafe_pcolor
  when (c == brown) $ do
               d <- unsafe_countdown
               atomic $ if (d <= 0)
                        then set_pcolor green >> set_countdown grass_regrowth_time
                        else set_countdown $ d -1



               
run ['setup, 'go]
