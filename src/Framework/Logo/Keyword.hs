{-# LANGUAGE TemplateHaskell #-}
module Framework.Logo.Keyword where

import Language.Haskell.TH
import Framework.Logo.Core
import Framework.Logo.Base
import Framework.Logo.Prim
import Control.Monad.Reader
import Control.Monad (liftM)
import Control.Concurrent.STM
import Data.Array
import Data.List (genericLength)


globals vs  = liftM2 (++) 
              -- trick to store the length of globals
              [d| globals_length = $(litE (integerL (genericLength vs))) |] 
              -- create 2 getters (1 prim and 1 unsafe) per global variable
              (liftM concat $ mapM (\ (v,i) -> do
                      p <- valD (varP (mkName v)) (normalB [| do (gs,_,_,_,_) <- ask :: CSTM Context; lift $ readTVar (gs ! $(litE (integerL i))) |]) []
                      u <- valD (varP (mkName ("unsafe_" ++ v))) (normalB [| do (gs,_,_,_,_) <- ask :: CIO Context; lift $ readTVarIO (gs ! $(litE (integerL i))) |]) []
                      return [p,u]
                    )  (zip vs [2..]))



turtles_own vs = [d| |]

patches_own vs = [d| |]

links_own vs = [d| |]

breeds_own bs vs = [d| |]

breeds [p,s] = sequence [valD (varP (mkName ("unsafe_" ++ p))) (normalB [| with (unsafe_breed >>= \ b -> return (b == $(litE (stringL p)))) =<< unsafe_turtles |]) []]

directed_link_breed [p,s] = [d| |]

undirected_link_breed [p,s] = [d| |]

link_breeds_own ls vs = [d| |]

run as = do 
  [d| main = do c <- cInit $(varE (mkName "globals_length")); runReaderT (foldl1 (>>) $(listE (map (\ a -> infixE (Just (varE a)) (varE (mkName ">>")) (Just (appE (varE (mkName "return")) (conE (mkName "()"))))) as))) c |]


