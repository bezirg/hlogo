-- Options: max-pxcor: 40, max-pycor: 40, hwrap, vwrap
import Language.Logo

globals ["sum_of_spins"]
patches_own ["spin"]

temperature = 2.24

run ["setup", "go"]

setup = do
  ask (do
         if ( -1 == 0) then atomic (set_spin =<< liftM head (one_of [-1,1])) else atomic (set_spin (-1))
         recolor
       ) =<< patches
  ss <- of_ spin =<< patches
  atomic $ set_sum_of_spins $ sum ss
  atomic $ reset_ticks

go = forever $ do
  t <- ticks
  when (t > 100000) stop
  ask update =<< unsafe_one_of =<< patches
  atomic $ tick

update = do
  s <- spin
  ns <- of_ spin =<< atomic neighbors4
  let ediff = 2 * s * sum ns
  rf <- unsafe_random_float 1.0
  when ((ediff <= 0) || (temperature > 0 && rf < exp ((- ediff) / temperature))) $ do
    atomic $ set_spin (-s)
    ns <- sum_of_spins
    atomic $ set_sum_of_spins (ns + 2 * s)
    recolor

recolor = do
  s <- spin
  atomic $ set_pcolor $ if s == 1 then blue+2 else blue-2
  
       


