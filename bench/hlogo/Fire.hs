-- | Options: max-pxcor: 125, max-pycor: 125, no-hwrap,no-vwrap
-- TODO: the stop condition should be that they are no turtles (fires&embers) left.
import Language.Logo

density = 99                    -- forest density
                                -- green patches are trees (black patches are rocks)

globals ["initial_trees", "burned_trees"]
breeds ["fires", "fire"]        -- fire turtles (each sprouted (ignited) upon a green tree)
breeds ["embers", "ember"]      -- ember turtles were fire turtles (in tick=-1...) and now they are fading their color to black. When they reach black they die
breeds_own "fires" []
breeds_own "embers" []

run ["setup", "go"]

setup = do
  ask (atomic $ set_pcolor green) =<< (with (liftM (< density) (unsafe_random_float 100)) =<< patches)
  ask ignite =<< (with (liftM2 (==) pxcor min_pxcor ) =<< patches) -- make an initial left column of burning trees
  sit <- count =<< (with (liftM (== green) pcolor) =<< patches)
  atomic $ set_initial_trees (fromIntegral sit)
  atomic $ set_burned_trees 0
  atomic $ reset_ticks

go = forever $ do
  ts <- ticks
  when (ts > 500) (stats_stm >>= unsafe_print >> stop)
  -- at each round, all current fires become embers and ask their green neighbour patches to ignite (create new fires)
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

