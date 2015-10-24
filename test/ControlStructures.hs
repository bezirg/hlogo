{-# LANGUAGE TemplateHaskell #-}

module ControlStructures where

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


controlstructuresTestGroup = $(testGroupGenerator)
case_Loop1 = let
    foo = loop (do
                 g <- glob1
                 when (g == 10) stop
                 atomic $ set_glob1 (g + 1))
             in runT $ do
                  foo
                  a1 <- glob1
                  let e1 = 10
                  lift $ e1 @=? a1

case_Foreach1 = runT $ do
  foreach [1,2,3] (\ x -> atomic $ crt x)
  a1 <- count =<< turtles
  let e1 = 6
  lift $ e1 @=? a1

case_While = runT $ do
  atomic $ random_seed 272
  atomic $ crt 10
  while (anyp =<< turtles) (ask (atomic $ die) =<< atomic (one_of =<< turtles))

  a1 <- anyp =<< turtles
  let e1 = False
  lift $ e1 @=? a1
  
caseIfElse = runT $ do
  if (2+2 == 4) then atomic $ crt 10 else atomic $ crt 20
  if (2+2 == 5) then atomic $ crt 3 else atomic $ crt 4
  
  a1 <- count =<< turtles
  let e1 = 14
  lift $ e1 @=? a1

case_RecursiveReporter1 = let
    fact :: Int -> CIO Int
    fact n = if n == 0
             then return 1
             else liftM (n *) $ fact (n-1)
                          in runT $ do
                            a1 <- fact 0
                            let e1 = 1
                            lift $ e1 @=? a1
                            
                            a2 <- fact 1
                            let e2 = 1
                            lift $ e2 @=? a2

                            a3 <- fact 2
                            let e3 = 2
                            lift $ e3 @=? a3

                            a4 <- fact 3
                            let e4 = 6
                            lift $ e4 @=? a4
                            
                            a5 <- fact 4
                            let e5 = 24
                            lift $ e5 @=? a5

                            a6 <- fact 5
                            let e6 = 120
                            lift $ e6 @=? a6
                            
