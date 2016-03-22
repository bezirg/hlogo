{-# LANGUAGE TemplateHaskell #-}

module ControlStructures (controlstructuresTestGroup) where

import Test.Tasty
import Test.Tasty.HUnit
import Utility

import Language.Logo
import Control.Monad.Trans.Class (lift)

globals ["glob1"]
breeds ["frogs", "frog"]
breeds ["mice", "mouse"]
breeds_own "frogs" []
breeds_own "mice" []
run [] -- workaround for tests

controlstructuresTestGroup =
 [testCase "case_Loop1" $ let
       foo = loop (do
                 g <- glob1
                 when (g == 10) stop
                 atomic $ set_glob1 (g + 1))
   in runT $ do
                  foo
                  a1 <- glob1
                  let e1 = 10
                  lift $ e1 @=? a1

 ,testCase "case_Foreach1" $ runT $ do
    ca
    foreach [1,2,3] (\ x -> crt x)
    a1 <- count =<< turtles
    let e1 = 6
    lift $ e1 @=? a1

 ,testCase "case_While" $ runT $ do
    random_seed 272
    crt 10
    while (anyp =<< turtles) (ask (atomic $ die) =<< (one_of =<< turtles))

    a1 <- anyp =<< turtles
    let e1 = False
    lift $ e1 @=? a1
  
 ,testCase "caseIfElse" $ runT $ do
    if (2+2 == 4) then crt 10 else crt 20
    if (2+2 == 5) then crt 3 else crt 4
  
    a1 <- count =<< turtles
    let e1 = 14
    lift $ e1 @=? a1

 ,testCase "case_RecursiveReporter1" $ let
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
 ]
