import Language.Logo
import Data.List (nub)

globals []
--globals ["total"]
patches_own ["n"]
turtles_own []

setup = do
  ask (atomic $ set_n 2 >> colorize) =<< patches
  c <- count =<< patches
  --atomic $ set_total $ 2 * fromIntegral c
  atomic $ reset_ticks

go = forever $ do
  ts <- ticks
  when (ts > 5000) $ stop
  active_patches <- unsafe_one_of =<< patches
  ask (atomic $ do
         with_n (+1)
         --with_total (+1)
         colorize) active_patches
  recurs active_patches
  atomic $ tick


recurs ap = do
        t <- anyp ap
        if t 
           then do
             overloaded_patches <- with (liftM (> 3) n) ap
             ask (do
                   atomic $ do
                     with_n (-4)
                     --with_total (-4)
                     colorize
                   ask (atomic $ do
                         with_n (+1)
                         --with_total (+1)
                         colorize) =<< neighbors4
                 ) overloaded_patches
             r <- (liftM (nub . concat) $ neighbors4 `of_` overloaded_patches)
             recurs r
           else return ()
  
colorize = do
  n_ <- n
  set_pcolor $ if n_ <= 0
               then 83
               else
                   if n_ <= 3
                   then item (truncate n_) [83,54,45,25]
                   else red
   
run ['setup, 'go]
