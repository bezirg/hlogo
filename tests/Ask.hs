{-# LANGUAGE TemplateHaskell, TupleSections #-}

module Ask (askTestGroup) where

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
import Prelude hiding (show)

globals ["glob1"]
turtles_own ["tvar"]
breeds ["frogs", "frog"]
breeds ["mice", "mouse"]
breeds_own "frogs" []
breeds_own "mice" []
run [] -- workaround for tests

askTestGroup = $(testGroupGenerator)
case_AskRNG_2D = runT $ do 
  atomic $ random_seed 0 -- not needed, because observer is initialized anyway with seed=0
  ca
  ask (atomic $ sprout 1) =<< atomic (one_of =<<  patches)
  ask (atomic $ sprout 1) =<< atomic (one_of =<<  patches)
  ask (atomic $ sprout 1) =<< atomic (one_of =<<  patches)
  ask (atomic $ sprout 1) =<< atomic (one_of =<<  patches)

  a1 <- of_ (atomic $ liftM4 (,,,) xcor ycor color heading)  =<< turtle 0
  let e1 = (14,13,95,224)
  lift $ e1 @=? a1
  a2 <- of_ (atomic $ liftM4 (,,,) xcor ycor color heading)  =<< turtle 1
  let e2 = (11,16,115,144)
  lift $ e2 @=? a2
  a3 <- of_ (atomic $ liftM4 (,,,) xcor ycor color heading)  =<< turtle 2
  let e3 = (8,8,75,62)
  lift $ e3 @=? a3
  a4 <- of_ (atomic $ liftM4 (,,,) xcor ycor color heading)  =<< turtle 3
  let e4 = (-6,-1,5,58)
  lift $ e4 @=? a4


-- case_AskRNG_2D_Nof = runT $ do 
--   atomic $ random_seed 0 -- not needed, because observer is initialized anyway with seed=0
--   ca
--   -- this is not the same as 4 times one_of! (above), because we delete successive draws from the agentset (so as not to return duplicates)
--   ask (atomic $ sprout 1) =<< atomic (n_of 4 =<< patches) 


--   let e1 = (14,13,95,224)
--   let e2 = (-12,9,75,287)
--   let e3 = (-16,-4,5,275)
--   let e4 = (9,13,135,150)

--   a1 <- of_ (atomic $ liftM4 (,,,) xcor ycor color heading)  =<< turtle 0
--   a2 <- of_ (atomic $ liftM4 (,,,) xcor ycor color heading)  =<< turtle 1
--   a3 <- of_ (atomic $ liftM4 (,,,) xcor ycor color heading)  =<< turtle 2
--   a4 <- of_ (atomic $ liftM4 (,,,) xcor ycor color heading)  =<< turtle 3
  
--   -- we have to do this because turtle-n ends up in different patches, since patches run in parallel
--   -- so we cannot associate a n-who of turtle to its attributes
--   lift $ assertBool "wrong attributes of turtles" $ null ([a1,a2,a3,a4]\\[e1,e2,e3,e4])

case_RecursiveCallInsideAsk1 = let
    go1 = do
      crt 1
      go2 5
      crt 1
    go2 :: Int -> C a b IO () -- sig. needed because of monomorphism restriction?
    go2 x =
      ask (do
             g <- glob1
             atomic $ set_glob1 (g + 1)
             when (x > 0) (go2 (x - 1))
           ) =<< turtle 0

                               in
                                 runT $ do
                                   ca
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
      crt 1
      go2
      crt 1
    go2 :: C a b IO () -- sig. needed because of monomorphism restriction?
    go2 = ask (do
                 g <- glob1
                 atomic $ set_glob1 (g + 1)
                 r <- atomic $ random (10 :: Int)
                 when (r > 0) go2) -- recurses until it reaches random=0
               =<< turtle 0
                                in runT $ do
                                  ca
                                  atomic $ set_glob1 0 -- not needed, because untyped (double) globals are initialized anyway to 0 
                                  atomic $ random_seed 0 -- not needed, because observer is initialized anyway with seed=0
                                  go1
                                          
                                  a1 <- count =<< turtles
                                  let e1 = 2
                                  lift $ e1 @=? a1

                                  a2 <- glob1
                                  let e2 = 10
                                  lift $ e2 @=? a2
          
case_RecursionOverAsk = let
    explore :: C Turtle a IO ()   -- sig. needed because of monomorphism restriction?
    explore = do
      t <- tvar
      when (t == 0) (do
                     atomic $ set_tvar 1
                     ns <- atomic $ neighbors
                     ask explore =<< turtles_on ns)
                        in runT $ do
                          ca
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
                                    crt 2
                                    a1 <- of_ foo =<< turtle 0
                                    let e1 = 10
                                    lift $ e1 @=? a1

                                    a2 <- glob1
                                    let e2 = 1
                                    lift $ e2 @=? a2

case_AskAllTurtles = runT $ do
  crt 1
  let a1 = ask (ask (atomic die) =<< turtles) =<< atomic (one_of =<< patches)
  --assertContextException (lift . evaluate =<< a1)

  let a2 = ask (ask (atomic die) =<< turtles) =<< atomic (one_of =<< turtles)
  --assertContextException (lift . evaluate =<< a2)
  lift $ assertFailure "HLogo does not have the ask limitation (Only the observer can ASK the set of all turtles or patches)"

case_AskAllPatches = runT $ do
  crt 1
                       
  let a1 = ask (ask (atomic $ sprout 1) =<< patches) =<< atomic (one_of =<< patches)
  --assertContextException (lift . evaluate =<< a1)

  let a2 = ask (ask (atomic $ sprout 1) =<< patches) =<< atomic (one_of =<< turtles)
  -- assertContextException (lift . evaluate =<< a2)

  lift $ assertFailure "HLogo does not have the ask limitation (Only the observer can ASK the set of all turtles or patches)"


case_AskObserverBlock = runT $ do
   reset_ticks
   atomic $ set_glob1 0
   crt 10
   ask (do
         wait 0.01
         t <- ticks
         when (t == 1) $ atomic $ set_glob1 1) =<< turtles
   tick
   wait 0.5
   a1 <- glob1
   let e1 = 0
   lift $ e1 @=? a1

case_AskNobody = runT $ do
   crt 2
   assertTypeException $ ask (do
                               ask (atomic die) =<< turtle 1
                               ask (show =<< self) =<< turtle 1 -- this should raise an exception to the parent since the agentref is nullified
                             ) =<< turtle 0                 
   
case_OfDie = runT $ do
  crt 2
  assertSomeException $ of_ (error "mplo" >> atomic die) =<< turtles
  
