import Framework.Logo

globals ["initial_trees", "burned_trees"]
patches_own []
turtles_own []
breeds ["fires", "fire"]
breeds ["embers", "ember"]
breeds_own "fires" []
breeds_own "embers" []

setup = do
  ask (atomic $ set_pcolor green) =<< (with (liftM (< 99) (unsafe_random_float 100)) =<< patches)
  ask ignite =<< (with (liftM2 (==) pxcor min_pxcor ) =<< patches)
  sit <- count =<< (with (liftM (== green) pcolor) =<< patches)
  atomic $ set_initial_trees (fromIntegral sit)
  atomic $ set_burned_trees 0
  atomic $ reset_ticks

go = forever $ do
  ts <- ticks
  when (ts > 500) (stop)
  ask (do
         ask ignite =<< (with (liftM (== green) pcolor) =<< atomic neighbors4)
         atomic $ set_breed "embers"
       ) =<< unsafe_fires
  fade_embers
  atomic $ tick

ignite = do
  s <- atomic $ sprout_fires 1
  ask (atomic $ set_color red) s
  atomic $ set_pcolor black
  atomic $ with_burned_trees (+1)

fade_embers = do
  ask (atomic $ do
          c <- liftM (\ x -> x - 0.3) color
          set_color c
          if c < red - 3.5 then set_pcolor c >> die else return ()) =<< unsafe_embers

run ['setup, 'go]
