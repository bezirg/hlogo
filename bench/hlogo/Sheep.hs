{-# LANGUAGE CPP #-}
-- Options: max-pxcor: 100, max-pycor: 100, hwrap, vwrap
-- turtles: sheep
-- patches: grass
-- sheep moves forward 1, eats grass
-- grass eating increases sheep's energy
-- sheep's moving decreases sheep's energy
-- grass grows back
-- no sheep dies
import Language.Logo

#ifndef NR_SHEEP
#define NR_SHEEP 10
#endif


patches_own ["countdown"]
breeds ["sheep", "a_sheep"]
breeds_own "sheep" ["senergy"]

-- Model Parameters
grassp = True
grass_regrowth_time = 30
initial_number_sheep = NR_SHEEP
sheep_gain_from_food = 4

run ["setup", "go"]

setup = do
  ask (atomic $ set_pcolor green) =<< patches
  when grassp $ ask (atomic $ do
                       r <- random grass_regrowth_time
                       c <- liftM head (one_of [green, brown])
                       set_countdown r
                       set_pcolor c
                     ) =<< patches

  s <- create_sheep initial_number_sheep
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
  reset_ticks


go = forever $ do
  t <- ticks
  when (t > 1000) (unsafe_sheep >>= count >>= print >> stop)
  ask (do
         move
         e <- senergy
         when grassp $ do
            atomic $ set_senergy (e -1)
            eat_grass
       ) =<< unsafe_sheep
  when grassp (ask grow_grass =<< patches)
  tick

move = atomic $ do
  r <- random 50
  l <- random 50
  rt r
  lt l
  fd 1

eat_grass = atomic $ do
  c <- pcolor
  when (c == green) $ do
              set_pcolor brown
              with_senergy (+ sheep_gain_from_food)

grow_grass = do
  c <- pcolor
  when (c == brown) $ do
               d <- countdown
               if (d <= 0)
               then atomic $ set_pcolor green >> set_countdown grass_regrowth_time
               else atomic $ set_countdown $ d -1



               

