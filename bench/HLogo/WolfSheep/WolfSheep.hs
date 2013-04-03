module Main where

import Framework.Logo.Keyword
import Framework.Logo.Prim
import Framework.Logo.Exception
import Framework.Logo.Base
import Control.Monad

globals ["grass"]
patches_own ["countdown"]
breeds ["wolves", "wolf"]
breeds ["sheep", "a_sheep"]
breeds_own "wolves" ["wenergy"]
breeds_own "sheep" ["senergy"]

-- Model Parameters
grassp = True
grass_regrowth_time = 30
initial_number_sheep = 100
initial_number_wolves = 50
sheep_gain_from_food = 4
wolf_gain_from_food = 20
sheep_reproduce = 4
wolf_reproduce = 5

setup = do
  ask_ (atomic $ set_pcolor green) =<< unsafe_patches
  when grassp $ ask_ (atomic $ do
                        set_countdown =<< random grass_regrowth_time
                        set_pcolor =<< liftM head (one_of [green, brown])
                     ) =<< unsafe_patches

  s <- atomic $ create_sheep initial_number_sheep
  ask_ (atomic $ do
          set_color white
          set_size 1.5
          set_label_color (blue -2)
          set_senergy =<< random (2 * sheep_gain_from_food)
          liftM2 setxy random_xcor random_ycor
       ) s

  w <- atomic $ create_wolves initial_number_wolves
  ask_ (atomic $ do
          set_color black
          set_size 2
          set_wenergy =<< random (2 * wolf_gain_from_food)
          liftM2 setxy random_xcor random_ycor
       ) w

  g <- count =<< with (liftM (== green) unsafe_pcolor) =<< unsafe_patches

  atomic $ set_grass (fromIntegral g)
  atomic $ reset_ticks

go = forever $ do
  t <- unsafe_ticks
  when (t > 1000) (unsafe_sheep >>= count >>= unsafe_show_ >> unsafe_wolves >>= count >>= unsafe_show_ >> stop)
  ask_ (do
         move
         e <- unsafe_senergy
         atomic $ set_senergy (e -1)
         eat_grass
         if (e-1 < 0) then (atomic $ die) else reproduce_sheep
       ) =<< unsafe_sheep
  ask_ (do
         move
         e <- unsafe_wenergy
         atomic $ set_wenergy (e-1)
         catch_sheep
         if (e-1 < 0) then (atomic $ die) else reproduce_wolves
       ) =<< unsafe_wolves
  when grassp (ask_ grow_grass =<< unsafe_patches)
  g <- count =<< with (liftM (== green) unsafe_pcolor) =<< unsafe_patches
  atomic $ set_grass (fromIntegral g)
  atomic $ tick

move = atomic $ do
         rt =<< random 50
         lt =<< random 50
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
                     ask_ (atomic $ (random_float 360 >>= rt >> fd 1)) w

reproduce_wolves = do
  r <- unsafe_random_float 100
  when (r < wolf_reproduce) $ do
                     w <- atomic $ with_wenergy (/ 2) >> hatch 1
                     ask_ (atomic $ (random_float 360 >>= rt >> fd 1)) w
                     
catch_sheep = do
  prey <- atomic $ one_of =<< sheep_here
  when (prey /= [Nobody]) $ do
                ask_ (atomic $ die) prey
                atomic $ with_wenergy (+ wolf_gain_from_food)

grow_grass = do
  c <- unsafe_pcolor
  when (c == brown) $ do
               d <- unsafe_countdown
               atomic $ if (d <= 0)
                        then set_pcolor green >> set_countdown grass_regrowth_time
                        else set_countdown $ d -1



               
run ['setup, 'go]
