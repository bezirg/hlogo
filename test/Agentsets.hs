{-# LANGUAGE TemplateHaskell #-}

module Agentsets where

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
turtles_own []
links_own []
breeds ["frogs", "frog"]
breeds ["mice", "mouse"]
breeds_own "frogs" []
breeds_own "mice" []

agentsetsTestGroup = $(testGroupGenerator)
case_Agentsets1 = runT $ do 

   atomic $ crt 10
   a1 <- atomic $ anyp =<< turtles
   let e1 = True
   lift $ e1 @=? a1

   ask (atomic $ die) =<< turtles
   a2 <- atomic $ anyp =<< turtles
   let e2 = False
   lift $ e2 @=? a2

   atomic $ crt 10
   a3 <- atomic $ anyp =<< turtles
   let e3 = True
   lift $ e3 @=? a3

   atomic $ ca
   a4 <- atomic $ anyp =<< turtles
   let e4 = False
   lift $ e4 @=? a4

   atomic $ crt 10
   a5 <- atomic $ anyp =<< turtles
   let e5 = True
   lift $ e5 @=? a5

   atomic $ ct
   a6 <- atomic $ anyp =<< turtles
   let e6 = False
   lift $ e6 @=? a6

   atomic $ crt 10
   a7 <- atomic $ anyp =<< turtles
   let e7 = True
   lift $ e7 @=? a7

   atomic $ cp
   a8 <- atomic $ anyp =<< turtles
   let e8 = True
   lift $ e8 @=? a8
   
   atomic $ ct
   atomic $ cp
   a9 <- atomic $ anyp =<< turtles
   let e9 = False
   lift $ e9 @=? a9

case_Agentsets2 = runT $ do
  let a1 = ask (atomic $ die) =<< atomic (one_of =<< turtles)
  
  assertTypeException (lift . evaluate =<< a1)

  atomic $ crt 10
  a2' <- atomic $ turtles
  a2 <- of_ (atomic $ who) =<< min_one_of a2' (atomic $ who)
  let e2 = 0
  lift $ e2 @=? head a2

  ask (atomic $ die) =<< with (liftM (<5) who) =<< turtles
  a3' <- atomic $ turtles
  a3 <- of_ (atomic $ who) =<< min_one_of a3' (atomic $ who)
  let e3 = 5
  lift $ e3 @=? head a3

  a4 <- of_ (atomic $ count =<< turtles_here) =<< turtle 7
  let e4 = 5
  lift $ e4 @=? head a4

  a5 <- of_ (atomic $ count =<< other =<< turtles_here) =<< turtle 7
  let e5 = 4
  lift $ e5 @=? head a5

  a6 <- of_ (atomic $ count =<< other =<< mice_here) =<< turtle 7
  let e6 = 0
  lift $ e6 @=? head a6

  a7 <- of_ (atomic $ count =<< other =<< frogs_here) =<< turtle 7
  let e7 = 0
  lift $ e7 @=? head a7

  ask (atomic $ set_breed "mice") =<< turtle 7
  ask (atomic $ set_breed "mice") =<< turtle 8
  a8 <- of_ (atomic $ count =<< mice_here) =<< turtle 7
  let e8 = 2
  lift $ e8 @=? head a8

  a9 <- of_ (atomic $ count =<< other =<< mice_here) =<< turtle 7
  let e9 = 1
  lift $ e9 @=? head a9

case_Agentsets3 = runT $ do
  atomic $ random_seed 18174
  
  a1 <- atomic $ count =<< n_of 0 =<< turtles
  let e1 = 0
  lift $ e1 @=? a1

  let a2 = atomic $ count =<< n_of 1 =<< turtles
  assertErrorCall (lift . evaluate =<< a2)

  let a3 = atomic $ count =<< n_of (-1) =<< patches
  assertErrorCall (lift . evaluate =<< a3)


  a4 <- atomic $ count =<< n_of 50 =<< patches
  a5 <- atomic $ count =<< n_of 50 =<< patches
  a6 <- atomic $ count =<< n_of 50 =<< patches
  a7 <- atomic $ count =<< n_of 50 =<< patches
  a8 <- atomic $ count =<< n_of 50 =<< patches
  let e4 = 50
  let e5 = 50
  let e6 = 50
  let e7 = 50
  let e8 = 50

  lift $ e4 @=? a4
  lift $ e5 @=? a5
  lift $ e6 @=? a6
  lift $ e7 @=? a7
  lift $ e8 @=? a8

case_Agentsets4_2D = runT $ do
  atomic $ random_seed 29020
  atomic $ crt 100
  ask (atomic $ fd 3) =<< turtles
  ask (atomic . create_links_with =<< with (do
                                              w1 <- who
                                              [w2] <- of_ who =<< myself
                                              return $ w1 > w2)  =<< turtles)
      =<< turtles
  a1 <- atomic $ count =<< links
  let e1 = 4950
  lift $ e1 @=? a1

  a2 <- atomic $ count =<< turtles
  e2 <- count =<< turtle_set [liftM concat $ of_ self =<< turtles]
  lift $ e2 @=? a2

  a3 <- atomic $ count =<< patches
  e3 <- count =<< patch_set [liftM concat $ of_ self =<< patches]
  lift $ e3 @=? a3

  a4 <- atomic $ count =<< links
  e4 <- count =<< link_set [liftM concat $ of_ self =<< links]
  lift $ e4 @=? a4

  a5 <- count =<< patch_set [liftM concat $ (of_ (atomic$ neighbors4) =<< patches)]
  e5 <- count =<< patches

  lift $ e5 @=? a5

  a6 <- count =<< with (atomic $ anyp =<< turtles_here) =<< patches
  e6 <- count =<< patch_set [liftM concat $ of_ (atomic $ patch_here) =<< turtles]
  lift $ e6 @=? a6

  a7 <- count =<< with (atomic $ anyp =<< turtles_here) =<< patches
  e7 <- count =<< turtle_set [liftM concat $ of_ (atomic $ one_of =<< turtles_here) =<< patches]
  lift $ e7 @=? a7

  a8 <- anyp =<< turtle_set [liftM concat $ of_ nobody =<< turtles]
  let e8 = False
  lift $ e8 @=? a8

  a9 <- anyp =<< patch_set [liftM concat $ of_ nobody =<< turtles]
  let e9 = False
  lift $ e9 @=? a9

  a10 <- anyp =<< link_set [liftM concat $ of_ nobody =<< turtles]
  let e10 = False
  lift $ e10 @=? a10


  a11 <- anyp =<< turtle_set [liftM concat $ of_ nobody =<< patches]
  let e11 = False
  lift $ e11 @=? a11

  a12 <- anyp =<< patch_set [liftM concat $ of_ nobody =<< patches]
  let e12 = False
  lift $ e12 @=? a12

  a13 <- anyp =<< link_set [liftM concat $ of_ nobody =<< patches]
  let e13 = False
  lift $ e13 @=? a13

  a14 <- anyp =<< turtle_set [liftM concat $ of_ nobody =<< links]
  let e14 = False
  lift $ e14 @=? a14

  a15 <- anyp =<< patch_set [liftM concat $ of_ nobody =<< links]
  let e15 = False
  lift $ e15 @=? a15

  a16 <- anyp =<< link_set [liftM concat $ of_ nobody =<< links]
  let e16 = False
  lift $ e16 @=? a16

  a17 <- anyp =<< turtle_set [liftM (concat) $ of_ (with (return False) =<< turtles) =<< turtles]
  let e17 = False
  lift $ e17 @=? a17

  a18 <- anyp =<< patch_set [liftM (concat) $ of_ (with (return False) =<< patches) =<< turtles]
  let e18 = False
  lift $ e18 @=? a18

  a19 <- anyp =<< link_set [liftM (concat) $ of_ (with (return False) =<< links) =<< turtles]
  let e19 = False
  lift $ e19 @=? a19

case_Agentsets5Box_2D = runT $ do
  atomic $ random_seed 3782
  -- requires at_points

case_AgentSetEquality = runT $ do
   a1 <- atomic turtles
   e1 <- atomic turtles
   lift $ e1 @=? a1

   a2 <- atomic links
   e2 <- atomic links
   lift $ e2 @=? a2

   a3 <- atomic turtles
   e3 <- atomic links
   lift $ e3 @=? a3

   a4 <- atomic no_turtles
   e4 <- atomic no_turtles
   lift $ e4 @=? a4

   a5 <- atomic no_patches
   e5 <- atomic no_patches
   lift $ e5 @=? a5

   a6 <- atomic no_links
   e6 <- atomic links
   lift $ e6 @=? a6

   a7 <- with (pxcor >>= \ px -> return $ px == 1000) =<< patches
   e7 <- atomic no_patches
   lift $ e7 @=? a7

   atomic $ crt 10
   ask (atomic . create_links_with =<< with (do
                                               w1 <- who
                                               [w2] <- of_ who =<< myself
                                               return $ w1 > w2)  =<< turtles)
      =<< turtles

   a8  <- atomic turtles
   e8 <- atomic no_turtles
   lift $ False @=? e8==a8

   a9 <- atomic links
   e9 <- atomic no_links
   lift $ False @=? e9==a9

   
   a10 <- with (liftM (blue ==) $ atomic color) =<< links
   e10 <- no_links
   lift $ e10 @=? a10

   a11 <- with (liftM (1000 ==) $ atomic who) =<< turtles
   e11 <- no_turtles
   lift $ e11 @=? a11

   a12 <- with (liftM (0 <=) $ atomic who) =<< turtles
   e12 <- atomic $ turtles
   lift $ e12 @=? a12

   a13 <- with (liftM (0 <=) $ atomic $ abs_ =<< pxcor) =<< patches
   e13 <- atomic $ patches
   lift $ e13 @=? a13

   a14 <- with (liftM (== 3) $ atomic who) =<< turtles
   e14 <- with (pxcor >>= \ x -> pycor >>= \ y -> return $ x == 3 && y == 3) =<< patches
   lift $ False @=? e14 == a14

   a15 <- with (liftM (== 3) $ atomic who) =<< turtles
   e15 <- with (liftM (== 3) $ atomic who) =<< turtles
   lift $ e15 @=? a15

   a16 <- with (liftM (== 3) $ atomic who) =<< turtles
   e16 <- with (liftM (== 4) $ atomic who) =<< turtles
   lift $ False @=? e16 == a16

   a17 <- with (liftM (> 5) $ atomic who) =<< turtles
   e17 <- with (liftM (> 5) $ atomic who) =<< turtles
   lift $ e17 @=? a17

   a18 <- with (liftM (> 5) $ atomic who) =<< turtles
   e18 <- with (liftM (< 5) $ atomic who) =<< turtles
   lift $ False @=? e18 == a18

   a19 <- with (atomic $ liftM2 (==) end1 (turtle 0)) =<< links
   e19 <- with (atomic $ liftM2 (==) end1 (turtle 0)) =<< links
   lift $ e19 @=? a19

   a20 <- with (pxcor >>= \ x -> pycor >>= \ y -> return $ x == 3 && y == 3) =<< patches
   e20 <- with (pxcor >>= \ x -> pycor >>= \ y -> return $ x == 3 && y == 3) =<< patches
   lift $ e20 @=? a20

   a21 <- no_turtles >>= \ x -> no_turtles >>= \ y -> return [x,y] >>= remove_duplicates
   e21 <- no_turtles
   lift $ [e21] @=? a21

   
case_SimpleLinkAgentset = runT $ do
  atomic $ crt 2
  ask (atomic $ create_link_to =<< turtle 1) =<< turtle 0
  ask (atomic $ create_link_from =<< turtle 1) =<< turtle 0
  ask (atomic die) =<< link 0 1

  a22 <- of_ (do
              [w1] <- of_ who =<< atomic end1
              [w2] <- of_ who =<< atomic end2
              return [w1,w2])
             =<< atomic (one_of =<< links)
  let e22 = [1,0]
  lift $ e22 @=? concat a22

case_CountTurtlesOptimization = runT $ do
  a1 <- count =<< with (return True) =<< turtles                             
  lift $ False @=? a1 > 0

  atomic $ crt 1
  a2 <- count =<< with (return True) =<< turtles                             
  lift $ True @=? a2 > 0
  
case_LinkAgentsetDeadLinks = runT $ do
  atomic $ crt 10
  ask (atomic $ create_links_with =<< other =<< turtles) =<< turtles
  ask (atomic $ die) =<< links

  a1 <- count =<< links
  let e1 = 0
  lift $ e1 @=? a1
