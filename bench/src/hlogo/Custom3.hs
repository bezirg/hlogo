import Language.Logo

globals []
patches_own []
turtles_own []

setup = do
  atomic $ create_turtles 100000
  atomic $ reset_ticks

go = forever $ do
  ask (behave) =<< turtles
  stop

behave = do
   atomic $ do 
           (forward 1 >> forward 1)
           (back 1 >> forward 1)
run ['setup, 'go]
