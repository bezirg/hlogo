module Main where

import Framework.Logo.Keyword
import Framework.Logo.Prim

globals ["g1", "g2", "g3"]
turtles_own ["t1", "t2"]
patches_own ["p1", "p2"]
links_own ["l1", "l2"]
breeds ["mice", "mouse"]
breeds_own "mice" ["b1", "b2"]
directed_link_breed ["arcs", "arc"]
undirected_link_breed ["edges", "edge"]
link_breeds_own "arcs" ["a1", "a2"]
link_breeds_own "edges" ["e1", "a2"]

setup = do
  atomic $ create_ordered_mice 100 -- 100000

go = do
  ask_ (atomic $ show_ =<< heading) =<< unsafe_mouse 17
  atomic $ show_ =<< count =<< mice
  -- ask_ behave =<< unsafe_turtles
  -- atomic $ create_mice 100
  -- unsafe_show_ =<< count =<< unsafe_mice
  -- unsafe_show_ =<< count =<< unsafe_turtles
  -- ask_ (atomic $ show_ =<< who) =<< unsafe_mouse 101
  -- ask_ (atomic $ show_ =<< who) =<< unsafe_mouse 99
  -- unsafe_show_ "ok"
  -- atomic $ show_ =<< g1
  -- atomic $ set_g1 5
  -- atomic $ show_ =<< g1



behave = do
    atomic (forward 1 >> forward 1)
    atomic (back 1 >> forward 1)


run ['setup, 'go]
