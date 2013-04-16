import Framework.Logo

globals []
patches_own ["countdown"]
breeds ["sheep", "a_sheep"]
breeds_own "sheep" ["senergy"]

-- Model Parameters
grassp = True
grass_regrowth_time = 30
initial_number_sheep = 1000
initial_number_wolves = 0
sheep_gain_from_food = 4
wolf_gain_from_food = 20
sheep_reproduce = 4
wolf_reproduce = 5

setup = do
  ask (atomic $ set_pcolor green) =<< unsafe_patches
  when grassp $ ask (atomic $ do
                       r <- random grass_regrowth_time
                       c <- liftM head (one_of [green, brown])
                       set_countdown r
                       set_pcolor c
                     ) =<< unsafe_patches

  s <- atomic $ create_sheep initial_number_sheep
  ask (atomic $ do
          s <- random (2 * sheep_gain_from_food)
          x <- random_xcor
          y <- random_ycor
          set_color white
          set_size 1.5
          set_label_color (blue -2)
          set_senergy s
          setxy x y
       ) s
  atomic $ reset_ticks


go = forever $ do
  t <- unsafe_ticks
  when (t > 1000) (unsafe_sheep >>= count >>= unsafe_print >> stop)
  ask (do
         move
         e <- unsafe_senergy
         when grassp $ do
            atomic $ set_senergy (e -1)
            eat_grass
       ) =<< unsafe_sheep
  when grassp (ask grow_grass =<< unsafe_patches)
  atomic $ tick

move = atomic $ do
  r <- random 50
  l <- random 50
  rt r
  lt l
  fd 1

eat_grass = do
  c <- unsafe_pcolor
  when (c == green) $ do
              atomic $ set_pcolor brown
              atomic $ with_senergy (+ sheep_gain_from_food)

grow_grass = do
  c <- unsafe_pcolor
  when (c == brown) $ do
               d <- unsafe_countdown
               atomic $ if (d <= 0)
                        then set_pcolor green >> set_countdown grass_regrowth_time
                        else set_countdown $ d -1



               
run ['setup, 'go]
