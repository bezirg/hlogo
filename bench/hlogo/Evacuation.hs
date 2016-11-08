{-# LANGUAGE TemplateHaskell, NoImplicitPrelude #-}
import Language.Logo

patches_own ["exit_distance"]
breeds ["pedestrians","pedestrian"]
breeds ["lights","light"]
breeds_own "pedestrians" []
breeds_own "lights" []

number_of_pedestrians = 1250

args = ["--max-pxcor=30"
       ,"--max-pycor=30"
       ,"--min-pxcor=-30"
       ,"--min-pycor=-30"
       ,"--horizontal-wrap=False"
       ,"--vertical-wrap=False"
       ]
run ["setup"]

setup = do
  create_environment

create_environment = do
  -- creating the environment
  ask (atomic $ set_pcolor lime) =<< with (liftM (> 27) pxcor) =<< patches
  ask (atomic $ set_pcolor blue) =<< with (liftM (== 27) pxcor) =<< patches
  ask (atomic $ set_pcolor red) =<<  with (do px <- pxcor 
                                              py <- pycor
                                              return $ (px == 27) && (py > -3) && (py < 3)) =<< patches
  --- setting distances from the exit (used later for moving the turtles)
  ask (do
        ps <- with (liftM (== red) pcolor ) =<< patches
        d <- distance =<< min_one_of ps (distance =<< myself)
        atomic $ set_exit_distance d
      ) =<< with (liftM (== black) pcolor) =<< patches
  ask (atomic $ set_exit_distance (-1)) =<< with (liftM (== lime) pcolor) =<< patches
  --- setup pedestrians
  setupPedestrians
  reset_ticks
  snapshot


setupPedestrians = do
 ts <- atomic $ create_pedestrians number_of_pedestrians
 ask (do 
       p <- unsafe_one_of =<< with (do
                   c <- pcolor
                   e <- exit_distance
                   a <- anyp =<< unsafe_pedestrians_here
                   return $ c == black && not a && e > 10
                 ) =<< patches
       atomic $ move_to p) ts

go = forever $ do
   ts <- anyp =<< unsafe_pedestrians                
   when (not ts) $ stop
   ask (atomic $ die) =<< with (liftM (== lime) pcolor) =<< unsafe_pedestrians -- They exit
   ask execute_behaviour =<< unsafe_pedestrians
   tick

execute_behaviour = do
  fn <- free_neighbors
  t <- min_one_of fn exit_distance
  when (t /= [Nobody]) $ atomic $ do
                      face t 
                      move_to t

free_neighbors = 
   with (do
          c <- pcolor
          e <- exit_distance
          p <- anyp =<< other =<< unsafe_pedestrians_here
          em <- liftM head $ (exit_distance `of_`) =<< myself
          return $ c /= blue && not p  && e < em) =<< neighbors


