{-# LANGUAGE TemplateHaskell #-}

module Timer (timerTestGroup) where

import Test.Framework.TH
import Test.Framework.Providers.HUnit
import Test.HUnit

import Language.Logo
import Control.Monad.Trans.Class (lift)

timerTestGroup = $(testGroupGenerator)
case_Init = runT $ do
              t <- timer 
              t' <- timer
              lift $ (t'-t >= 0) @? "should be bigger or equal to 0"
case_Wait = runT $ do
              t <- timer 
              wait 0.1
              t' <- timer
              lift $ (t'-t >= 0.1) @? "should be bigger or equal to 0.1"
