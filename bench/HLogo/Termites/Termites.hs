module Main where

import Framework.Logo.Keyword
import Framework.Logo.Prim
import Framework.Logo.Exception
import Framework.Logo.Base
import Control.Monad
import System.Exit
import Control.Monad.Trans.Class (lift)

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
  ask_ (do
         x <- unsafe_random_xcor
         y <- unsafe_random_ycor
         atomic $ do
           set_color white
           setxy x y
           set_size 5) ts
  atomic $ reset_ticks


go = do
  ask_ (do 
        check_ticks
      ) =<< unsafe_turtles
  unsafe_print_ "reached"

check_ticks = forever $ do
  t <- unsafe_ticks
  when (t > 400) (do
                    unsafe_show_ t
                    unsafe_show_ =<< count =<< unsafe_turtles
                    stop
                  )
  w <- atomic $ who
  unsafe_print_ t
  search_for_chip
  find_new_pile
  put_down_chip
  atomic $ tick

search_for_chip = do
  c <- unsafe_pcolor
  if (c == yellow)
    then atomic $ do
      set_pcolor black
      set_color orange
      fd 20
    else do
      wiggle
      search_for_chip

find_new_pile = do
  c <- unsafe_pcolor
  when (c /= yellow) $ do
                  wiggle
                  find_new_pile

put_down_chip = do
  c <- unsafe_pcolor
  if (c == black) 
    then do
      atomic $ do
           set_pcolor yellow
           set_color white
      get_away
    else do
      r <- unsafe_random 360
      atomic $ rt r >> fd 1
      put_down_chip
    
get_away = do
  r <- unsafe_random 360
  atomic $ do
    rt r
    fd 20
  c <- unsafe_pcolor
  when (c /= black) get_away

wiggle = do
  r1 <- unsafe_random 50
  r2 <- unsafe_random 50
  atomic $ do
           fd 1
           rt r1
           lt r2

run ['setup, 'go]
