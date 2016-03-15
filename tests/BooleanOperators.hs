{-# LANGUAGE TemplateHaskell #-}

module BooleanOperators (booleanoperatorsTestGroup) where

import Test.Framework.TH
import Test.Framework.Providers.HUnit
import Test.HUnit
import Utility

import Language.Logo
import Control.Monad.Trans.Class (lift)


globals ["glob1"]
turtles_own []
links_own []
breeds ["frogs", "frog"]
breeds ["mice", "mouse"]
breeds_own "frogs" []
breeds_own "mice" []


booleanoperatorsTestGroup = $(testGroupGenerator)
case_ShortCircuitAnd = runT $ do
  atomic $ set_glob1 0
  g <- glob1
  let a1 = g == 3 && 1 / g == 0
  let e1 = False
  lift $ e1 @=? a1

  let a2 = g == 0 && 1 / g == 0
  assertSomeException (lift $ evaluate a2)

case_ShortCircuitOr = runT $ do
  atomic $ set_glob1 0
  g <- glob1
  let a1 = g == 0 || 1 / g == 0
  let e1 = True
  lift $ e1 @=? a1

  let a2 = g == 3 || 1 / g == 0
  assertSomeException (lift $ evaluate a2)

