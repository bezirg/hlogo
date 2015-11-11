-- Options: max-pxcor: 100, max-pycor: 100, hwrap, vwrap
import Language.Logo

#ifndef NR_TURTLES
#define NR_TURTLES 10
#endif

run ["setup", "go"]

setup = do
  ask (atomic $ do
         [c] <- one_of [black, black, black, black, black, black, black, black, red, blue]
         set_pcolor c) =<< patches
  atomic $ create_turtles NR_TURTLES
  reset_ticks

go = forever $ do
  t <- ticks
  when (t==10) $ stop
  x <- of_ behave =<< turtles
  print x
  tick

behave = do
  c <- pcolor
  atomic $ fd 1 >> if c == red
                   then lt 30
                   else when (c == blue) (rt 30)




