import Language.Logo

patches_own []
turtles_own ["flockmates", "nearest_neighbor"]

population = 1
vision = 3
minimum_separation = 1
max_align_turn = 5
max_cohere_turn = 3
max_separate_turn = 1.5

setup = do
  ts <- atomic $ crt population
  ask (do
          r <- unsafe_random 7
          atomic $ set_color $ yellow - 2 + r
          atomic $ set_size 1.5
          x <- unsafe_random_xcor
          y <- unsafe_random_ycor
          atomic $ setxy x y
       ) ts
  atomic $ reset_ticks

go = forever $ do
  t <- ticks
  when (t > 10000) stop
  ask flock =<< turtles
  repeat_ 5 (ask (atomic $ fd 0.2) =<< turtles)
  atomic $ tick

flock = do
  fm <- find_flockmates
  a <- anyp fm 
  when a $ do
          n <- find_nearest_neighbor fm
          d <- distance n
          if d < minimum_separation then separate n else (align fm >> cohere fm)

find_flockmates = do
  t <- turtles
  fm <- in_radius t vision
  other fm

find_nearest_neighbor fm = min_one_of fm (distance =<< myself)


separate n = do
  [h] <- (of_ heading n) 
  turn_away h max_separate_turn

align fm = do
  afh <- average_flockmate_heading fm
  turn_towards afh  max_align_turn

average_flockmate_heading fm = do
  x <- of_ dx fm
  y <- of_ dy fm
  let x_component = sum x
  let y_component = sum y
  if x_component == 0 && y_component == 0 then heading else return $ atan_ x_component y_component

cohere fm = do
  aht <- average_heading_towards_flockmates fm
  turn_towards aht max_cohere_turn

average_heading_towards_flockmates fm = do
  x <- of_ (do 
            t <- towards =<< myself
            return $ sin_ (t + 180)) fm
  y <- of_ (do 
            t <- towards =<< myself
            return $ cos_ (t + 180)) fm
  let x_component = mean x
  let y_component = mean y
  if x_component == 0 && y_component == 0 then heading else return $ atan_ x_component y_component
    

turn_towards nh mt = do
  h <- heading
  sh <- subtract_headings nh h
  turn_at_most sh mt

turn_away nh mt = do
  h <- heading
  sh <- subtract_headings h nh
  turn_at_most sh mt

turn_at_most turn max_turn = atomic $ if abs turn > max_turn
                                      then if turn > 0
                                           then rt max_turn
                                           else lt max_turn
                                      else rt turn



run ['setup, 'go]
