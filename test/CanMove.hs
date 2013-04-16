{-# LANGUAGE TemplateHaskell #-}

module CanMove where

import Framework.Logo.Keyword
import Framework.Logo.Prim
import Framework.Logo.Base
import Framework.Logo.Exception
import Control.Monad.Trans.Class
import Test.Framework
import Test.Framework.TH
import Test.Framework.Providers.HUnit
import Test.HUnit
import Control.Monad
import Data.List
import Utility

globals ["glob1"]
patches_own []
turtles_own []
links_own []
breeds ["frogs", "frog"]
breeds ["mice", "mouse"]
breeds_own "frogs" []
breeds_own "mice" []


canmoveTestGroup = $(testGroupGenerator)
case_CanMove2Box_2D = runT $ do
  -- box topology
  atomic $ crt 1
  ask (atomic $ set_heading 0 >> fd 5.1) =<< unsafe_turtle 0

  [a1] <- of_ (atomic $ can_movep 12) =<< unsafe_turtle 0
  let e1 = False
  lift $ e1 @=? a1

  [a2] <- of_ (atomic $ can_movep 11.6) =<< unsafe_turtle 0
  let e2 = False
  lift $ e2 @=? a2

  [a3] <- of_ (atomic $ can_movep 11.3) =<< unsafe_turtle 0
  let e3 = True
  lift $ e3 @=? a3
