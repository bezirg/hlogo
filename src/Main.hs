module Main where

import Framework.Logo.Core
import Framework.Logo.Prim
import qualified Framework.Logo.Prim.Unsafe as Unsafe
import Control.Monad.Reader
import Framework.Logo.Base
import Control.Exception (evaluate)
import Data.List (foldl')

main = do
  c <- cInit
  runReaderT (setup >> go) c

setup = do
  atomic $ create_turtles 100000

go = do
  ask_ behave =<< Unsafe.turtles
  Unsafe.show_ "ok"

behave = do
    atomic (forward 1 >> forward 1)
    atomic (back 1 >> forward 1)
