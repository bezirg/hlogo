{-# LANGUAGE TemplateHaskell, NoImplicitPrelude #-}
import Language.Logo

run ["setup", "go"]

setup = do
  crt 1000
  reset_ticks

go = forever $ do
  ts <- ticks
  when (ts>100) $ do
    print =<< stats_stm
    stop
  ask (atomic $ set_pcolor =<< color) =<< turtles -- they try to set at the color of patch(0,0) altogether with their own color
  tick