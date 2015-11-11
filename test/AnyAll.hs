{-# LANGUAGE TemplateHaskell #-}

module AnyAll (anyallTestGroup) where

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
breeds ["frogs", "frog"]
breeds ["mice", "mouse"]
breeds_own "frogs" []
breeds_own "mice" []
run [] -- workaround for tests

anyallTestGroup = $(testGroupGenerator)
case_All1 = runT $ do 
  a1 <- allp (return True) =<< patches
  let e1 = True
  lift $ e1 @=? a1

  a2 <- allp (return False) =<< patches
  let e2 = False
  lift $ e2 @=? a2

  a3 <- allp (return True) =<< turtles
  let e3 = True
  lift $ e3 @=? a3

  a4 <- allp (return False) =<< turtles
  let e4 = True
  lift $ e4 @=? a4

case_All2 = runT $ do
  crt 1
  a1 <- allp (return False) =<< turtles
  let e1 = False
  lift $ e1 @=? a1

case_All3_2D = runT $ do
  a1 <- allp (atomic $ liftM2 (==) (patch 0 0) self) =<< patches
  let e1 = False
  lift $ e1 @=? a1

  a2 <- allp (atomic $ liftM2 (==) (patch_at 0 0) self) =<< patches
  let e2 = True
  lift $ e2 @=? a2

case_All4 = runT $ do
  ask (atomic $ sprout 1) =<< patches
  a1 <- allp (do
              s <- self
              th <- atomic $ one_of =<< turtles_here
              s' <- of_ (atomic $ patch_here) th 
              return $ s == concat s') =<< patches
  let e1 = True
  lift $ e1 @=? a1

case_AnyOptimizations = runT $ do
  a1 <- atomic $ anyp =<< turtles
  let e1 = False
  lift $ e1 @=? a1

  a2 <- liftM not $ atomic $ anyp =<< turtles
  let e2 = True
  lift $ e2 @=? a2

  a3 <- atomic $ count =<< turtles
  let e3 = 0
  lift $ e3 @=? a3

  a4 <- atomic $ count =<< turtles
  let e4 = 0
  lift $ False @=? e4 /= a4
  
  a5 <- atomic $ count =<< turtles
  let e5 = 0
  lift $ False @=? e5 < a5

  crt 1

  a6 <- atomic $ anyp =<< turtles
  let e6 = True
  lift $ e6 @=? a6

  a7 <- liftM not $ atomic $ anyp =<< turtles
  let e7 = False
  lift $ e7 @=? a7

  
  a8 <- atomic $ count =<< turtles
  let e8 = 0
  lift $ True @=? e8 < a8
  
