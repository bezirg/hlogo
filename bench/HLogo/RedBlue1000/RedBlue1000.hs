import Framework.Logo

globals []
patches_own []
turtles_own []

setup = do
  ask (do
         [c] <- unsafe_one_of [black, black, black, black, black, black, black, black, red, blue]
         atomic $ set_pcolor c) =<< patches
  atomic $ create_turtles 1000
  atomic $ reset_ticks

go = forever $ do
  t <- ticks
  when (t==1000) $ stop
  ask (behave) =<< turtles
  atomic $ tick

behave = do
  c <- pcolor
  atomic $ fd 1 >> if c == red
                   then lt 30
                   else when (c == blue) (rt 30)

run ['setup, 'go]
