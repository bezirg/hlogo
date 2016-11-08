{-# LANGUAGE TemplateHaskell, NoImplicitPrelude #-}
import Language.Logo

args = ["--max-pxcor=100"
       ,"--max-pycor=100"
       ,"--min-pxcor=-100"
       ,"--min-pycor=-100"
       ,"--horizontal-wrap=False"
       ,"--vertical-wrap=False"
       ]
run ["setup", "go"]

setup = do
  create_turtles 100000
  reset_ticks

go = forever $ do
  t <- ticks
  when (t==8) $ stop
  ask behave =<< turtles
  show t
  tick

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



