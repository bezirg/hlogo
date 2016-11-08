{-# LANGUAGE TemplateHaskell, NoImplicitPrelude #-}

-- turtles: termites
-- patches: wood chips
import Language.Logo

density = 20
number = 400

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
    r <- random_float 100
    when (r < density) $ atomic $ set_pcolor yellow) =<< patches

  ts <- create_turtles number
  ask (do
    atomic $ set_color white
    x <- random_xcor
    y <- random_ycor
    atomic $ setxy x y
    atomic $ set_size 5) ts
  reset_ticks

go = forever $ do
  t <- ticks
  when (t > 100) stop
  ask (do 
        search_for_chip
        find_new_pile
        put_down_chip
      ) =<< turtles
  tick


search_for_chip = do
  success <- atomic $ do
    c <- pcolor
    if (c == yellow)
    then do -- pickup the chip atomically
      set_pcolor black
      return True
    else return False
  if success
    then do
      atomic $ set_color orange
      atomic $ fd 20
    else do
      wiggle
      search_for_chip

find_new_pile = do
  c <- pcolor
  when (c /= yellow) $ do
      wiggle
      find_new_pile

put_down_chip = do
  success <- atomic $ do
    c <- pcolor
    if (c == black) 
    then do
      set_pcolor yellow
      return True
    else
      return False     
  if success
    then do
      atomic $ set_color white
      get_away
    else do
      (atomic . rt) =<< random 360
      atomic $ fd 1
      put_down_chip
    
get_away = do
  (atomic . rt) =<< random 360
  atomic $ fd 20
  c <- pcolor
  when (c /= black) get_away

wiggle = do
  atomic $ fd 1
  (atomic . rt) =<< random 50
  (atomic . lt) =<< random 50

