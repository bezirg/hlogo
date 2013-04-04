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
  t <- atomic $ crt 1
  ask_ (atomic $ do
          set_color white
          set_heading 0
          fd 16.5
          show_ =<< ycor
          fd 1
          show_ =<< ycor
       ) t
  unsafe_show_ $ horizontal_wrap_ conf
  unsafe_show_ $ vertical_wrap_ conf
  atomic $ reset_ticks
  snapshot

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


run ['setup] --, 'go]
