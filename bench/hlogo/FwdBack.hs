-- Options: max-pxcor: 100 , max-pycor: 100, no-hwrap, no-vwrap
import Language.Logo

patches_own []
turtles_own []

setup = do
  atomic $ create_turtles 100000
  atomic $ reset_ticks

go = forever $ do
  t <- ticks
  when (t==8) $ stop
  ask (behave) =<< turtles
  unsafe_show t
  atomic $ tick

behave = do
   atomic $ do 
           (forward 1 >> forward 1)
           (back 1 >> forward 1)

   atomic $ do 
           (forward 1 >> forward 1)
           (back 1 >> forward 1)

   atomic $ do 
           (forward 1 >> forward 1)
           (back 1 >> forward 1)

   atomic $ do 
           (forward 1 >> forward 1)
           (back 1 >> forward 1)


run ['setup, 'go]