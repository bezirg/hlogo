{-# LANGUAGE TemplateHaskell #-}

module AgentsetBuilding (agentsetbuildingTestGroup) where

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

agentsetbuildingTestGroup = $(testGroupGenerator)
case_TurtleSet_2D = runT $ do 
               crt 1
                      
               e1 <- of_ (atomic $ turtle_set [self]) =<< turtle 0
               a1 <- turtles
               lift $ e1 @=? a1

               crt 10
               -- e2 <- atomic $ turtle_set [sort_ =<< turtles, turtle 0]
               -- a2 <- turtles
               -- lift $ [] @=? e2 \\ a2
              
               -- e3 <- atomic $ turtle_set [turtles, turtle 0]
               -- a3 <- turtles
               -- lift $ [] @=? e3 \\ a3

               let e5 = [4,6,9]
               a5 <- sort_ =<< of_ who =<< atomic (turtle_set [turtle 6, turtle 4, turtle 9])
               
               lift $ e5 @=? a5

               let e6 = 1
               a6 <- count =<< turtle_set [turtle 0, turtle 0]
               
               lift $ e6 @=? a6

               create_frogs 5
               create_mice 7

               return ()
               -- let e7 = 12
               -- a7 <- count =<< turtle_set [unsafe_frogs, unsafe_mice]
               -- lift $ e7 @=? a7

case_TurtleSet_3D = assertFailure "HLogo does not currently support 3D lattices"

case_EmptyTurtleSet = runT $ do
   a1 <- atomic $ turtle_set []                     
   e1 <- atomic $ no_turtles
   lift $ e1 @=? a1

   a2 <- atomic $ turtle_set [nobody, nobody]
   e2 <- atomic $ no_turtles
   
   lift $ a2 @=? e2

case_TurtleSetNestedLists = assertFailure "HLogo currently does not support flattening deeply nested agentsets"

case_PatchSet2_2D = runT $ do
   crt 1

   ask (atomic $ set_glob1 3) =<< patch 0 0
   a2 <- atomic $ glob1
   let e2 = 3
   lift $ e2 @=? a2
   
   a3 <- atomic $ one_of =<< patch_set [patch 0 0]
   e3 <- atomic $ patch 0 0
   lift $ e3 @=? a3

   -- a4 <- atomic $ patches
   -- e4 <- atomic $ patch_set [patch 0 0, patches]
   -- lift $ [] @=? e4 \\ a4 

   -- a5 <- atomic $ patches
   -- e5 <- atomic $ patch_set [patch 0 0, sort_ =<< patches]
   -- lift $ [] @=? e5 \\ a5

   a7 <-  sort_ =<< of_ pxcor =<< atomic (patch_set [patch 3 0, patch 1 0, patch 2 0])
   let e7 = [1,2,3]
   lift $ e7 @=? a7

   a8 <-  sort_ =<< of_ pycor =<< atomic (patch_set [patch 3 0, patch 1 0, patch 2 0])
   let e8 = [0,0,0]
   lift $ e8 @=? a8
   
   a8 <- atomic $ count =<< (patch_set [patch 0 0, patch 0 0])
   let e8 = 1
   lift $ e8 @=? a8

case_PatchSet2_3D = assertFailure "HLogo does not currently support 3D lattices"

case_EmptyPatchSet = runT $ do
   a1 <- atomic $ patch_set []
   e1 <- atomic $ no_patches
   lift $ e1 @=? a1

   a2 <- atomic $ patch_set [nobody,  nobody]
   e2 <- atomic $ no_patches
   lift $ e2 @=? a2

   
case_PatchSetNestedLists_2D = assertFailure "HLogo currently does not support flattening deeply nested agentsets"

case_PatchSetNestedLists_3D = assertFailure "HLogo does not currently support 3D lattices"

case_LinkSet_2D = runT $ do
   crt 3
   ask (atomic $ create_link_with =<< turtle 1) =<< turtle 0
   
   let a1 = of_ (atomic $ link_set [self]) =<< atomic (link 0 2)
   assertTypeException (lift . evaluate =<< a1)

   -- TODO: bug here, be careful with undirected links and equality
   a2 <- of_ (atomic $ link_set [self]) =<< atomic (link 0 1)
   e2 <- atomic $ links
   lift $ e2 @=? a2


   -- ask (atomic $ create_links_with =<< other =<< turtles) =<< turtles
   -- -- a3 <- atomic $ link_set [sort_ =<< links, link 0 1]
   -- -- e3 <- atomic $ links
   -- -- lift $ [] @=? a3 \\ e3

   -- -- a6 <- atomic $ link_set [links, link 0 1]
   -- -- e6 <- atomic $ links
   -- -- lift $ [] @=? a6 \\ e6

   -- a7 <- atomic $ count =<< link_set [link 1 2, link 1 2]
   -- let e7 = 1
   -- lift $ e7 @=? a7


case_LinkSet_3D = assertFailure "HLogo does not currently support 3D lattices"

case_EmptyLinkSet = runT $ do
   a1 <- atomic $ link_set []
   e1 <- atomic $ no_links
   lift $ e1 @=? a1

   a2 <- atomic $ link_set [nobody, nobody]
   e2 <- atomic $ no_links
   lift $ e2 @=? a2

case_LinkSetNestedLists =  assertFailure "HLogo currently does not support flattening deeply nested agentsets"
                            
