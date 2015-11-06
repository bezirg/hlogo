-- Options: max-pxcor: 100, max-pycor: 100, hwrap, vwrap
-- each patch has a current temp which diffuses over time because of leakage
-- bugs (turtles) have an ideal temp
-- their unhappiness is the abs (ideal temp - on-patch-temp)
-- every step they may move to a nearest patch neighbour that will offer them less unhappiness. They also emit some heat
-- bugs cannot stack
import Language.Logo

globals ["color_by_unhappinessq"]
turtles_own ["ideal_temp", "output_heat", "unhappiness"]
patches_own ["temp"]

bug_count = 100
min_ideal_temp = 10
max_ideal_temp = 40
min_output_heat = 5
max_output_heat = 25
diffusion_rate = 0.9
evaporation_rate = 0.01
random_move_chance = 0

setup = do
  atomic $ set_color_by_unhappinessq 0     -- false
  ask (do
        s <- atomic $ sprout 1
        ask (atomic $ do
               rt <- random (max_ideal_temp - min_ideal_temp)
               set_ideal_temp $ min_ideal_temp + rt
               rh <- random (max_output_heat - min_output_heat)
               set_output_heat $ min_output_heat + rh
               i <- ideal_temp
               t <- temp
               set_unhappiness $ abs (i - t)
               color_by_ideal_temp
               face =<< one_of =<< neighbors
               set_size 2
            ) s
      ) =<< unsafe_n_of bug_count =<< patches
  -- snapshot
  atomic $ reset_ticks

color_by_ideal_temp = do
 let range_adjustment = (max_ideal_temp - min_ideal_temp) / 2
 set_color =<< scale_color lime ideal_temp (min_ideal_temp - range_adjustment) (max_ideal_temp + range_adjustment)
     

color_by_unhappiness max_unhappiness = do
  set_color =<< scale_color blue unhappiness max_unhappiness 0

go = forever $ do
  t <- ticks
  when (t > 10) stop
  diffuse temp set_temp diffusion_rate
  -- ask (atomic $ with_temp (\ t -> t * (1 - evaporation_rate))) =<< patches
  -- ask step =<< turtles 
  recolor_turtles
  recolor_patches
  atomic $ tick
          
recolor_turtles = do
  c <- color_by_unhappinessq
  when (c == 1) $ do
    max_unhappiness <- (return . max_) =<< of_ (atomic $ unhappiness) =<< turtles
    ask (atomic $ color_by_unhappiness max_unhappiness) =<< turtles
       
recolor_patches = do
  ask (atomic $ set_pcolor =<< scale_color red temp 0 150) =<< patches

step = do
  i <- ideal_temp
  t <- temp
  let u = abs (i - t)
  atomic $ set_unhappiness u
  when (u > 0) $ do
         r <- unsafe_random_float 100
         if r < random_move_chance
           then bug_move =<< unsafe_one_of =<< neighbors
           else bug_move =<< best_patch 
  o <- output_heat
  atomic $ with_temp (+ o)
          
best_patch = do
  t <- temp
  i <- ideal_temp
  if (t < i)
    then do
      winner <- (\ ns -> max_one_of ns (return t)) =<< neighbors
      [wt] <- of_ temp winner
      if (wt > t)
        then return winner
        else patch_here
    else do
      winner <- (\ ns -> min_one_of ns (return t)) =<< neighbors
      [wt] <- of_ temp winner
      if (wt < t)
        then return winner
        else patch_here

bug_move target = do
  p <- patch_here
  if (target == p)
    then return ()
    else do
      p <- anyp =<< turtles_on target
      if not p
        then do
          atomic (do face target
                     move_to target
                 )
        else do
          target' <- unsafe_one_of =<< with (liftM not (anyp =<< turtles_here)) =<< neighbors
          when (target /= [Nobody]) $ atomic $ move_to target'

run ['setup] --, 'go]
