-- Options: max-pxcor: 100 , max-pycor: 100, no-hwrap, no-vwrap
import Language.Logo

run ["setup", "go"]

setup = do
  atomic $ create_turtles 100000
  atomic $ reset_ticks

go = forever $ do
  t <- ticks
  when (t==8) $ stop
  ask (behave) =<< turtles
  show t
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



