import Framework.Logo

globals []
patches_own []
turtles_own []

density = 20
number = 400

setup = do
  ask (do
         r <- unsafe_random_float 100
         when (r < density) $ atomic $ set_pcolor yellow) =<< unsafe_patches

  ts <- atomic $ create_turtles number
  ask (do
         x <- unsafe_random_xcor
         y <- unsafe_random_ycor
         atomic $ do
           set_color white
           setxy x y
           set_size 5) ts
  atomic $ reset_ticks

go = forever $ do
  t <- unsafe_ticks
  when (t > 100) (do
                    unsafe_show t
                    unsafe_show =<< count =<< unsafe_turtles
                    stop
                  )
  ask (do 
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
