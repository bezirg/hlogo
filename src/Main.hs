module Main where

import Framework.Logo.Keyword
import Framework.Logo.Prim
import Framework.Logo.Exception
import Framework.Logo.Base
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
  t <- atomic $ crt 100
  ask_ (atomic $ do
          x <- random_xcor
          y <- random_xcor
          setxy x y
       ) t
  atomic $ reset_ticks

go = do
  ask_ (atomic $ do 
          px <- pxcor
          py <- pycor
          when (even px && even py) (set_pcolor yellow))
      =<< unsafe_patches
  snapshot

behave = do
    atomic (forward 1 >> forward 1)
    atomic (back 1 >> forward 1)


run ['setup, 'go]
