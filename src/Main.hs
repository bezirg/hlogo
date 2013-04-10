{-# LANGUAGE TemplateHaskell #-}
module Main where

import Framework.Logo.Keyword
import Framework.Logo.Prim
import Framework.Logo.Exception
import Framework.Logo.Base
import Framework.Logo.Conf
import Control.Monad

globals ["g1", "g2", "g3"]
turtles_own ["t1", "t2"]
patches_own ["p1", "p2"]
links_own ["l1", "l2"]
breeds ["mice", "mouse"]
breeds_own "mice" ["b1", "b2"]
directed_link_breed ["arcs", "arc"]
undirected_link_breed ["edges", "edge"]
link_breeds_own "arcs" ["a1", "a2"]
link_breeds_own "edges" ["e1", "e2"]

setup = do
  return ()

go = do
  atomic $ crt 10
  a2' <- atomic $ turtles
  a2 <- of_ (atomic $ who) =<< min_one_of a2' (atomic $ who)
  unsafe_show_ $ head a2

  unsafe_show_ =<< with (who >>= \ w -> return (w < 5)) =<< unsafe_turtles

behave = do
    atomic (forward 1 >> forward 1)
    atomic (back 1 >> forward 1)


run ['setup, 'go]
