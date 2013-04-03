module Main where

import Framework.Logo.Keyword
import Framework.Logo.Prim
import Framework.Logo.Exception
import Framework.Logo.Base
import Control.Monad

globals []
patches_own []
turtles_own []

density = 20
number = 400

setup = do
  ask_ (do
         r <- unsafe_random_float 100
         when (r < density) $ atomic $ set_pcolor yellow) =<< unsafe_patches

  ts <- atomic $ create_turtles number
  ask_ (atomic $ do
          set_color white
          liftM2 setxy random_xcor random_ycor
          set_size 5) ts
  atomic $ reset_ticks


go = forever $ do
  t <- unsafe_ticks
  when (t > 1000) (do
                    unsafe_show_ t
                    unsafe_show_ =<< count =<< unsafe_turtles
                    stop
                  )
  ask_ (do 
        search_for_chip
        find_new_pile
        put_down_chip
      ) =<< unsafe_turtles
  atomic $ tick


search_for_chip = do
  c <- unsafe_pcolor
  if (c == yellow)
    then atomic $ do
      set_pcolor black
      set_color orange
      fd 20
    else do
      atomic $ wiggle
      search_for_chip

find_new_pile = do
  c <- unsafe_pcolor
  when (c /= yellow) $ do
                  atomic $ wiggle
                  find_new_pile

put_down_chip = do
  c <- unsafe_pcolor
  if (c == black) 
    then atomic $ do
      set_pcolor yellow
      set_color white
      get_away
    else do
      r <- unsafe_random 360
      atomic $ rt (fromIntegral r) >> fd 1
      put_down_chip
    
get_away = do
  rt =<< random 360
  fd 20
  c <- pcolor
  when (c /= black) get_away

wiggle = do
  fd 1
  rt =<< random 50
  lt =<< random 50

run ['setup, 'go]
