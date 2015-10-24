{-# LANGUAGE TemplateHaskell #-}

module BooleanOperators where

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
patches_own []
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

