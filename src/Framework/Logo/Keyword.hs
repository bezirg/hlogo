{-# LANGUAGE TemplateHaskell #-}
module Framework.Logo.Keyword where

import Language.Haskell.TH
import Framework.Logo.Core
import Control.Monad.Reader


globals vs  = [d| |]

turtles_own vs = [d| |]

patches_own vs = [d| |]

links_own vs = [d| |]

breeds_own bs vs = [d| |]

breed [p,s] = [d| |]

directed_link_breed [p,s] = [d| |]

undirected_link_breed [p,s] = [d| |]

link_breeds_own ls vs = [d| |]

run as = do 
  [d| main = do c <- cInit; runReaderT (foldl1 (>>) $(listE (map (\ a -> infixE (Just (varE a)) (varE (mkName ">>")) (Just (appE (varE (mkName "return")) (conE (mkName "()"))))) as))) c |]


