{-# LANGUAGE TemplateHaskell #-}

module CanMove (canmoveTestGroup) where

import Test.Framework.TH
import Test.Framework.Providers.HUnit
import Test.HUnit

import Language.Logo
import Control.Monad.Trans.Class (lift)

globals ["glob1"]
breeds ["frogs", "frog"]
breeds ["mice", "mouse"]
breeds_own "frogs" []
breeds_own "mice" []
run [] -- workaround for tests

canmoveTestGroup = $(testGroupGenerator)
case_CanMove2Box_2D = runT $ do
  -- box topology
  crt 1
  ask (atomic $ set_heading 0 >> fd 5.1) =<< turtle 0

  a1 <- of_ (atomic $ ycor) =<< turtle 0
  let e1 = 5.1
  lift $ e1 @=? a1

  a1 <- of_ (atomic $ can_movep 12) =<< turtle 0
  let e1 = False
  lift $ e1 @=? a1

  a2 <- of_ (atomic $ can_movep 11.6) =<< turtle 0
  let e2 = False
  lift $ e2 @=? a2

  a3 <- of_ (atomic $ can_movep 11.3) =<< turtle 0
  let e3 = True
  lift $ e3 @=? a3
