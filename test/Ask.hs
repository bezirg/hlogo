{-# LANGUAGE TemplateHaskell, TupleSections #-}

module Ask where

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
turtles_own ["tvar"]
links_own []
breeds ["frogs", "frog"]
breeds ["mice", "mouse"]
breeds_own "frogs" []
breeds_own "mice" []


askTestGroup = $(testGroupGenerator)
case_AskRNG_2D = runT $ do 
  atomic $ random_seed 0
  unsafe_wait 0.1  
  -- ask_ (atomic $ sprout 1) =<< atomic (n_of 4 =<< patches)
  --  cannot use this because is non deterministic, ask STM
  -- instead
  ask_ (atomic $ sprout 1) =<< atomic (one_of =<<  patches)
  ask_ (atomic $ sprout 1) =<< atomic (one_of =<<  patches)
  ask_ (atomic $ sprout 1) =<< atomic (one_of =<<  patches)
  ask_ (atomic $ sprout 1) =<< atomic (one_of =<<  patches)

  a1 <- of_ (atomic $ liftM4 (,,,) xcor ycor color heading)  =<< unsafe_turtle 0
  let e1 = [(0,-5,95,225)]
  lift $ e1 @=? a1

  a2 <- of_ (atomic $ liftM4 (,,,) xcor ycor color heading)  =<< unsafe_turtle 1
  let e2 = [(12,14,105,225)]
  lift $ e2 @=? a2


  a3 <- of_ (atomic $ liftM4 (,,,) xcor ycor color heading)  =<< unsafe_turtle 2
  let e3 = [(5,-4,55,268)]
  lift $ e3 @=? a3

  a4 <- of_ (atomic $ liftM4 (,,,) xcor ycor color heading)  =<< unsafe_turtle 3
  let e4 = [(-14,-1,35,195)]
  lift $ e4 @=? a4


case_RecursiveCallInsideAsk1 = let
    go1 = do
      atomic $ crt 1
      go2 5
      atomic $ crt 1
    go2 x =
      ask_ (do
             g <- unsafe_glob1
             atomic $ set_glob1 (g + 1)
             when (x > 0) (go2 (x - 1))
           ) =<< unsafe_turtle 0

                               in
                                 runT $ do
                                   atomic $ set_glob1 0
                                   go1
                                   a1 <- count =<< unsafe_turtles
                                   let e1 = 2
                                   lift $ e1 @=? a1

                                   a2 <- unsafe_glob1
                                   let e2 = 6
                                   lift $ e2 @=? a2

case_RecursiveCallInsideAsk2 = let
    go1 = do
      atomic $ crt 1
      go2
      atomic $ crt 1
    go2 = ask_ (do
                 g <- unsafe_glob1
                 atomic $ set_glob1 (g + 1)
                 r <- atomic $ random (10 :: Int)
                 when (r > 0) go2) =<< unsafe_turtle 0
                                in runT $ do
                                  atomic $ set_glob1 0
                                  atomic $ random_seed 0
                                  go1
                                          
                                  a1 <- count =<< unsafe_turtles
                                  let e1 = 2
                                  lift $ e1 @=? a1

                                  a2 <- unsafe_glob1
                                  let e2 = 21
                                  lift $ e2 @=? a2
          
case_RecursionOverAsk = let
    explore = do
      t <- unsafe_tvar
      when (t == 0) (do
                        atomic $ set_tvar 1
                        ns <- atomic $ neighbors
                        ask_ explore =<< turtles_on ns)
                        in runT $ do
                          ask_ (atomic $ sprout 1) =<< unsafe_patches
                          ask_ explore =<< atomic (one_of =<< turtles)
                          a1 <- anyp =<< with (liftM (== 0) unsafe_tvar) =<< unsafe_turtles
                          let e1 = False
                          lift $ e1 @=? a1

case_AskInsideReporterProcedure = let
    foo = do
      ask_ (atomic $ set_glob1 =<< liftM fromIntegral who) =<< unsafe_turtle 1
      return 10
                                  in runT $ do
                                    atomic $ crt 2
                                    [a1] <- of_ foo =<< unsafe_turtle 0
                                    let e1 = 10
                                    lift $ e1 @=? a1
                                    
                                    a2 <- unsafe_glob1
                                    let e2 = 1
                                    lift $ e2 @=? a2

case_AskAllTurtles = runT $ do
  atomic $ crt 1
  let a1 = ask_ (ask_ (atomic die) =<< unsafe_turtles) =<< atomic (one_of =<< patches)
  --assertContextException (lift . evaluate =<< a1)

  let a2 = ask_ (ask_ (atomic die) =<< unsafe_turtles) =<< atomic (one_of =<< turtles)
  --assertContextException (lift . evaluate =<< a2)
  lift $ assertFailure "HLogo does not have the ask limitation (Only the observer can ASK the set of all turtles or patches)"

case_AskAllPatches = runT $ do
  atomic $ crt 1
                       
  let a1 = ask_ (ask_ (atomic $ sprout 1) =<< unsafe_patches) =<< atomic (one_of =<< patches)
  --assertContextException (lift . evaluate =<< a1)

  let a2 = ask_ (ask_ (atomic $ sprout 1) =<< unsafe_patches) =<< atomic (one_of =<< turtles)
  -- assertContextException (lift . evaluate =<< a2)

  lift $ assertFailure "HLogo does not have the ask limitation (Only the observer can ASK the set of all turtles or patches)"
