import Framework.Logo

globals ["sum_of_spins"]
patches_own ["spin"]
turtles_own []

temperature = 2.24

setup = do
  ask (do
         if ( -1 == 0) then atomic (set_spin =<< liftM head (one_of [-1,1])) else atomic (set_spin (-1))
         recolor
       ) =<< unsafe_patches
  ss <- of_ unsafe_spin =<< unsafe_patches
  atomic $ set_sum_of_spins $ sum ss
  atomic $ reset_ticks

go = forever $ do
  t <- unsafe_ticks
  when (t > 100000) stop
  ask update =<< unsafe_one_of =<< unsafe_patches
  atomic $ tick

update = do
  s <- unsafe_spin
  ns <- of_ unsafe_spin =<< atomic neighbors4
  let ediff = 2 * s * sum ns
  rf <- unsafe_random_float 1.0
  when ((ediff <= 0) || (temperature > 0 && rf < exp_ ((- ediff) / temperature))) $ do
    atomic $ set_spin (-s)
    ns <- unsafe_sum_of_spins
    atomic $ set_sum_of_spins (ns + 2 * s)
    recolor

recolor = do
  s <- unsafe_spin
  atomic $ set_pcolor $ if s == 1 then blue+2 else blue-2
  
       

run ['setup, 'go]
