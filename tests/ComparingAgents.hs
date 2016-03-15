{-# LANGUAGE TemplateHaskell #-}

module ComparingAgents (comparingagentsTestGroup) where

import Test.Framework.TH
import Test.Framework.Providers.HUnit
import Test.HUnit

import Language.Logo
import Control.Monad.Trans.Class (lift)

globals ["glob1"]
breeds ["frogs", "frog"]
breeds ["mice", "mouse"]
breeds_own "frogs" []
breeds_own "mice" []
run [] -- workaround for tests


comparingagentsTestGroup = $(testGroupGenerator)
-- case_ComparingLinks = runT $ do
--   crt 3
--   ask (atomic $ fd 5) =<< turtles
--   ask (atomic $ create_links_to =<< other =<< turtles) =<< turtle 0
--   ask (atomic $ create_links_to =<< other =<< turtles) =<< turtle 1
--   ask (atomic $ create_links_with =<< other =<< turtles) =<< turtle 0
  
--   a1 <- atomic $ liftM2 (>) (link 1 0) (link 0 1)
--   let e1 = True
--   lift $ e1 @=? a1

--   a2 <- atomic $ liftM2 (>) (link 0 1) (link 0 1)
--   let e2 = False
--   lift $ e2 @=? a2

--   a3 <- atomic $ liftM2 (<) (link 0 1) (link 0 1)
--   let e3 = False
--   lift $ e3 @=? a3

--   a4 <- atomic $ liftM2 (>=) (link 0 1) (link 0 1)
--   let e4 = True
--   lift $ e4 @=? a4

--   a5 <- atomic $ liftM2 (<=) (link 0 1) (link 0 1)
--   let e5 = True
--   lift $ e5 @=? a5
  

case_ComparingTurtles = runT $ do
  crt 2

  a1 <- atomic $ liftM2 (>) (turtle 0) (turtle 1)
  let e1 = False
  lift $ e1 @=? a1

  a2 <- atomic $ liftM2 (<) (turtle 0) (turtle 1)
  let e2 = True
  lift $ e2 @=? a2

  a3 <- atomic $ liftM2 (<=) (turtle 0) (turtle 1)
  let e3 = True
  lift $ e3 @=? a3

  a4 <- atomic $ liftM2 (>=) (turtle 0) (turtle 1)
  let e4 = False
  lift $ e4 @=? a4

  a5 <- atomic $ liftM2 (<=) (turtle 0) (turtle 0)
  let e5 = True
  lift $ e5 @=? a5

  a6 <- atomic $ liftM2 (>=) (turtle 0) (turtle 0)
  let e6 = True
  lift $ e6 @=? a6

  a7 <- atomic $ liftM2 (<) (turtle 0) (turtle 0)
  let e7 = False
  lift $ e7 @=? a7

  a8 <- atomic $ liftM2 (>) (turtle 0) (turtle 0)
  let e8 = False
  lift $ e8 @=? a8

case_ComparingPatches_2D = runT $ do
  a1 <- atomic $ liftM2 (>) (patch 0 0) (patch 0 1)
  let e1 = True
  lift $ e1 @=? a1

  a2 <- atomic $ liftM2 (>=) (patch 0 0) (patch 0 1)
  let e2 = True
  lift $ e2 @=? a2

  
  a3 <- atomic $ liftM2 (<) (patch 0 0) (patch 0 1)
  let e3 = False
  lift $ e3 @=? a3

  a4 <- atomic $ liftM2 (<=) (patch 0 0) (patch 0 1)
  let e4 = False
  lift $ e4 @=? a4

  a5 <- atomic $ liftM2 (>=) (patch 0 0) (patch 1 0)
  let e5 = False
  lift $ e5 @=? a5

  a6 <- atomic $ liftM2 (<) (patch 0 0) (patch 1 0)
  let e6 = True
  lift $ e6 @=? a6

  a7 <- atomic $ liftM2 (<=) (patch 0 0) (patch 1 0)
  let e7 = True
  lift $ e7 @=? a7

  a8 <- atomic $ liftM2 (>) (patch 0 0) (patch 0 0)
  let e8 = False
  lift $ e8 @=? a8

  a9 <- atomic $ liftM2 (>=) (patch 0 0) (patch 0 0)
  let e9 = True
  lift $ e9 @=? a9

  a10 <- atomic $ liftM2 (<) (patch 0 0) (patch 0 0)
  let e10 = False
  lift $ e10 @=? a10

  a11 <- atomic $ liftM2 (<=) (patch 0 0) (patch 0 0)
  let e11 = True
  lift $ e11 @=? a11
