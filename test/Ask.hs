{-# LANGUAGE TemplateHaskell, TupleSections #-}

module Ask where

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
turtles_own ["tvar"]
links_own []
breeds ["frogs", "frog"]
breeds ["mice", "mouse"]
breeds_own "frogs" []
breeds_own "mice" []


askTestGroup = $(testGroupGenerator)
case_AskRNG_2D = runT $ do 
  atomic $ random_seed 0
  wait 0.1  
  -- ask_ (atomic $ sprout 1) =<< atomic (n_of 4 =<< patches)
  --  cannot use this because is non deterministic, ask STM
  -- instead
  ask (atomic $ sprout 1) =<< atomic (one_of =<<  patches)
  ask (atomic $ sprout 1) =<< atomic (one_of =<<  patches)
  ask (atomic $ sprout 1) =<< atomic (one_of =<<  patches)
  ask (atomic $ sprout 1) =<< atomic (one_of =<<  patches)

  a1 <- of_ (atomic $ liftM4 (,,,) xcor ycor color heading)  =<< turtle 0
  let e1 = [(0,-5,65,309)]
  lift $ e1 @=? a1

  a2 <- of_ (atomic $ liftM4 (,,,) xcor ycor color heading)  =<< turtle 1
  let e2 = [(-3,-2,85,204)]
  lift $ e2 @=? a2


  a3 <- of_ (atomic $ liftM4 (,,,) xcor ycor color heading)  =<< turtle 2
  let e3 = [(-6,-10,45,148)]
  lift $ e3 @=? a3

  a4 <- of_ (atomic $ liftM4 (,,,) xcor ycor color heading)  =<< turtle 3
  let e4 = [(12,14,35,52)]
  lift $ e4 @=? a4


case_RecursiveCallInsideAsk1 = let
    go1 = do
      atomic $ crt 1
      go2 5
      atomic $ crt 1
    go2 x =
      ask (do
             g <- glob1
             atomic $ set_glob1 (g + 1)
             when (x > 0) (go2 (x - 1))
           ) =<< turtle 0

                               in
                                 runT $ do
                                   atomic $ set_glob1 0
                                   go1
                                   a1 <- count =<< turtles
                                   let e1 = 2
                                   lift $ e1 @=? a1

                                   a2 <- glob1
                                   let e2 = 6
                                   lift $ e2 @=? a2

case_RecursiveCallInsideAsk2 = let
    go1 = do
      atomic $ crt 1
      go2
      atomic $ crt 1
    go2 = ask (do
                 g <- glob1
                 atomic $ set_glob1 (g + 1)
                 r <- atomic $ random (10 :: Int)
                 when (r > 0) go2) =<< turtle 0
                                in runT $ do
                                  atomic $ set_glob1 0
                                  atomic $ random_seed 0
                                  go1
                                          
                                  a1 <- count =<< turtles
                                  let e1 = 2
                                  lift $ e1 @=? a1

                                  a2 <- glob1
                                  let e2 = 1
                                  lift $ e2 @=? a2
          
case_RecursionOverAsk = let
    explore = do
      t <- tvar
      when (t == 0) (do
                        atomic $ set_tvar 1
                        ns <- atomic $ neighbors
                        ask explore =<< turtles_on ns)
                        in runT $ do
                          ask (atomic $ sprout 1) =<< patches
                          ask explore =<< atomic (one_of =<< turtles)
                          a1 <- anyp =<< with (liftM (== 0) tvar) =<< turtles
                          let e1 = False
                          lift $ e1 @=? a1

case_AskInsideReporterProcedure = let
    foo = do
      ask (atomic $ set_glob1 =<< liftM fromIntegral who) =<< turtle 1
      return 10
                                  in runT $ do
                                    atomic $ crt 2
                                    [a1] <- of_ foo =<< turtle 0
                                    let e1 = 10
                                    lift $ e1 @=? a1
                                    
                                    a2 <- glob1
                                    let e2 = 1
                                    lift $ e2 @=? a2

case_AskAllTurtles = runT $ do
  atomic $ crt 1
  let a1 = ask (ask (atomic die) =<< turtles) =<< atomic (one_of =<< patches)
  --assertContextException (lift . evaluate =<< a1)

  let a2 = ask (ask (atomic die) =<< turtles) =<< atomic (one_of =<< turtles)
  --assertContextException (lift . evaluate =<< a2)
  lift $ assertFailure "HLogo does not have the ask limitation (Only the observer can ASK the set of all turtles or patches)"

case_AskAllPatches = runT $ do
  atomic $ crt 1
                       
  let a1 = ask (ask (atomic $ sprout 1) =<< patches) =<< atomic (one_of =<< patches)
  --assertContextException (lift . evaluate =<< a1)

  let a2 = ask (ask (atomic $ sprout 1) =<< patches) =<< atomic (one_of =<< turtles)
  -- assertContextException (lift . evaluate =<< a2)

  lift $ assertFailure "HLogo does not have the ask limitation (Only the observer can ASK the set of all turtles or patches)"


case_AskNobody = runT $ do
   atomic $ crt 2
   assertTypeException $ ask (do
                               ask (atomic die) =<< turtle 1
                               ask (unsafe_show_ =<< self) =<< turtle 1 -- this should raise an exception to the parent since the agentref is nullified
                             ) =<< turtle 0                 
   
case_OfDie = runT $ do
  atomic $ crt 2
  assertSomeException $ of_ (error "mplo" >> atomic die) =<< turtles
  
