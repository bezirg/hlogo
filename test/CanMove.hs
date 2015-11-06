{-# LANGUAGE TemplateHaskell #-}

module CanMove where

import Language.Logo.Keyword
import Language.Logo.Prim
import Language.Logo.Base
import Language.Logo.Exception
import Control.Monad.Trans.Class
import Test.Framework
import Test.Framework.TH
import Test.Framework.Providers.HUnit
import Test.HUnit
import Control.Monad
import Data.List
import Utility

globals ["glob1"]
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
  ask (atomic $ set_heading 0 >> fd 5.1) =<< turtle 0

  [a1] <- of_ (atomic $ can_movep 12) =<< turtle 0
  let e1 = False
  lift $ e1 @=? a1

  [a2] <- of_ (atomic $ can_movep 11.6) =<< turtle 0
  let e2 = False
  lift $ e2 @=? a2

  [a3] <- of_ (atomic $ can_movep 11.3) =<< turtle 0
  let e3 = True
  lift $ e3 @=? a3
