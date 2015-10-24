import Language.Logo

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
  ask (atomic $ set_pcolor green) =<< patches
  when grassp $ ask (do
                       r <- unsafe_random grass_regrowth_time
                       c <- liftM head (unsafe_one_of [green, brown])
                       atomic $ do
                         set_countdown r
                         set_pcolor c
                     ) =<< patches

  s <- atomic $ create_sheep initial_number_sheep
  ask (do
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
  t <- ticks
  when (t > 1000) (unsafe_sheep >>= count >>= unsafe_print_ >> stop)
  ask (do
         move
         e <- senergy
         when grassp $ do
            atomic $ set_senergy (e -1)
            eat_grass
         if (e-1 < 0 && grassp) then (atomic die) else reproduce_sheep
       ) =<< unsafe_sheep
  when grassp (ask grow_grass =<< patches)
  atomic $ tick

move = do
  r <- unsafe_random 50
  l <- unsafe_random 50
  atomic $ do
         rt r
         lt l
         fd 1

eat_grass = do
  c <- pcolor
  when (c == green) $ do
              atomic $ set_pcolor brown
              atomic $ with_senergy (+ sheep_gain_from_food)

reproduce_sheep = do
  r <- unsafe_random_float 100
  when (r < sheep_reproduce) $ do
                    w <- atomic $ with_senergy (/ 2) >> hatch 1
                    ask ((unsafe_random_float 360 >>= \ r -> atomic (rt r >> fd 1))) w

grow_grass = do
  c <- pcolor
  when (c == brown) $ do
               d <- countdown
               atomic $ if (d <= 0)
                        then set_pcolor green >> set_countdown grass_regrowth_time
                        else set_countdown $ d -1



               
run ['setup, 'go]
