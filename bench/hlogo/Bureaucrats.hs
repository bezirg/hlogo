{-# LANGUAGE TemplateHaskell, NoImplicitPrelude #-}
-- | Imagine an N-by-N grid of office desks and a bureaucrat sitting at each. A folder
-- is randomly assigned to one desk. The bureaucrat does nothing until four or more folders are
-- on his desk at which time he sends one to each of his four nearest neighbors. Any bureaucrat
-- sitting at the edge of this array throws a folder out the window if there is no desk to send
-- it to. Sometimes, adding one folder can cause multiple redistributions of folders as one
-- bureaucrat’s actions causes neighbors to exceed three folders, which then ripples through
-- the office. In principle, just adding one new folder might involve redistribution at every
-- desk, sometimes multiple times.
-- A yellow cell indicates two folders on the desk, a blue cell is zero, green one, and red three folders. The 100x100 model will automatically stop after 5000 model steps if you leave it running.
-- Setup to 2 folders per bureaucrat.
-- At each go, one random bureaucrat is sent one new folder.
-- Options: max-pxcor: 99, max-pycor: 99, no-hwrap,no-vwrap
-- Adapted from Bureaucrats-fast.nlogo
import Language.Logo
import qualified Data.Vector as V (singleton,concat, toList, fromList)
import Data.List (nub)

globals ["total"]
patches_own ["n"]


args = ["--max-pxcor=50"
       ,"--max-pycor=50"
       ,"--min-pxcor=-50"
       ,"--min-pycor=-50"
       ,"--horizontal-wrap=False"
       ,"--vertical-wrap=False"
       ]
run ["setup", "go"]

setup = do
  ask (atomic $ do
    set_n 2
    colorize) =<< patches
  c <- count =<< patches
  atomic $ set_total $ 2 * fromIntegral c
  reset_ticks

go = forever $ do
  ts <- ticks
  when (ts > 5000) $ do
    -- print =<< count =<< with (liftM (== 0) n) =<< patches
    -- print =<< count =<< with (liftM (== 1) n) =<< patches
    -- print =<< count =<< with (liftM (== 2) n) =<< patches
    -- print =<< count =<< with (liftM (== 3) n) =<< patches
    -- print =<< count =<< with (liftM (> 3) n) =<< patches
    print =<< total
    --snapshot
    stop
  active_patches <- one_of =<< patches
  -- add a folder in every go
  ask (atomic $ do
         with_n (+1)
         with_total (+1)
         colorize) active_patches
  -- maybe forward, RIPPLE EFFECT
  recurse $ V.singleton active_patches
  
  tick


recurse ap = do
  t <- anyp ap
  when t $ do
    overloaded_patches <- with (liftM (> 3) n) ap
    --print =<< count overloaded_patches
    ask (do
      atomic $ do
          with_n (subtract 4)
          with_total (subtract 4)
          colorize
      ask (atomic $ do
                with_n (+1)
                with_total (+1)
                colorize) =<< neighbors4
      ) overloaded_patches
    recurse =<< liftM patch_set_ (neighbors4 `of_` overloaded_patches)
  
colorize = do
  n_ <- n
  set_pcolor $ if n_ <= 0
               then 83
               else
                   if n_ <= 3
                   then item (truncate n_) [83,54,45,25]
                   else red
   
-- this operation is the most expensive and makes it slow
patch_set_ lps = V.fromList . nub . V.toList $ V.concat lps
-- nub is O(n^2)

--patch_set_ lps = V.fromList . nub . concat $ map V.toList lps