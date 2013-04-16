{-# LANGUAGE TemplateHaskell #-}

module Breeds where

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


breedsTestGroup = $(testGroupGenerator)
case_TestIsBreed = runT $ do
  a1 <- atomic $ is_frogp =<< nobody
  let e1 = False
  lift $ e1 @=? a1

  a2 <- atomic $ is_frogp =<< turtle 0
  let e2 = False
  lift $ e2 @=? a2

  atomic $ create_turtles 1

  a3 <- atomic $ is_frogp =<< turtle 0
  let e3 = False
  lift $ e3 @=? a3
  
  atomic $ create_frogs 1

  a4 <- atomic $ is_frogp =<< turtle 1
  let e4 = True
  lift $ e4 @=? a4


  a5 <- atomic $ is_mousep =<< turtle 1
  let e5 = False
  lift $ e5 @=? a5

  ask (atomic $ die) =<< unsafe_turtle 1
  
  a6 <- atomic $ is_frogp =<< turtle 1
  let e6 = False
  lift $ e6 @=? a6

  a7 <- atomic $ is_mousep =<< turtle 1
  let e7 = False
  lift $ e7@=? a7

  a8 <- atomic $ is_frogp (55 :: Int)
  let e8 = False
  lift $ e8 @=? a8

case_IsLinkBreed = runT $ do
  a1 <- atomic $ is_directed_linkp =<< nobody
  let e1 = False
  lift $ e1 @=? a1

  a2 <- atomic $ is_directed_linkp =<< link 0 1
  let e2 = False
  lift $ e2 @=? a2

  atomic $ crt 2
  ask (atomic $ create_link_to =<< turtle 1) =<< unsafe_turtle 0

  a3 <- atomic $ is_directed_linkp =<< link 0 1
  let e3 = True
  lift $ e3 @=? a3

  
case_SetBreedToNonBreed = runT $ do
  atomic $ crt 1
  ask (atomic $ set_breed "turtles") =<< unsafe_turtle 0

  atomic $ crt 1
  ask (atomic $ set_breed "frogs") =<< unsafe_turtle 1

  atomic $ crt 1
  ask (atomic $ set_breed "patches") =<< unsafe_turtle 2

  atomic $ crt 1
  ask (atomic $ set_breed "links") =<< unsafe_turtle 3


  lift $ assertFailure "No run-time checking of the breed type and value on setting"
