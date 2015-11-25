{-# LANGUAGE LambdaCase, CPP #-}
{-# OPTIONS_HADDOCK show-extensions #-}
-- | 
-- Module      :  Language.Logo.Prim
-- Copyright   :  (c) 2013-2015, the HLogo team
-- License     :  BSD3
-- Maintainer  :  Nikolaos Bezirgiannis <bezirgia@cwi.nl>
-- Stability   :  experimental
--
-- This module tries to provide an API to the standard library of NetLogo:
-- <http://ccl.northwestern.edu/netlogo/docs/dictionary.html>
module Language.Logo.Prim (
                            -- * Agent related
                            self, myself, other, count, nobody, towards, allp, at_points, towardsxy, in_cone, every, wait, carefully, die, 

                            -- * Turtle related
                            turtles_here, turtles_at, turtles_on, jump, setxy, forward, fd, back, bk, turtles, turtle, turtle_set, face, xcor, set_breed, with_breed, set_color, with_color, set_label_color, with_label_color, with_label, set_xcor, heading, set_heading, with_heading,  ycor, set_ycor, who, color, breed, dx, dy, home, right, rt, left, lt, downhill, downhill4, hide_turtle, ht, show_turtle, st, pen_down, pd, pen_up, pu, pen_erase, pe, no_turtles, hatch, set_size, with_size, with_shape,

                            -- * Patch related
                            patch_at, patch_here, patch_ahead, patches, patch, patch_set, no_patches, pxcor, pycor,pcolor,  neighbors, neighbors4, set_plabel, with_plabel, set_pcolor, with_pcolor, with_plabel_color, 

                            -- * Link related
                            hide_link, show_link, link_length, link, links, my_links, my_out_links, my_in_links, no_links, tie, untie, link_set, end1, end2, 

                            -- * Random related
                            random_xcor, random_ycor, random_pxcor, random_pycor, random, random_float, new_seed, random_seed, random_exponential, random_gamma, random_normal, random_poisson,

                            -- * Color
                            black, white, gray, red, orange, brown, yellow, green, lime, turquoise, cyan, sky, blue, violet, magenta, pink, scale_color, extract_rgb, approximate_rgb,

                            -- * List related
                            sum, anyp, item, one_of, min_one_of, max_one_of, remove, remove_item, replace_item, shuffle, sublist, substring, n_of, butfirst, butlast, emptyp, first, foreach, fput, last, length, list, lput, map, memberp, position, reduce, remove_duplicates, reverse, sentence, sort_, sort_by, sort_on, max_, min_,n_values, is_listp, is_stringp, word,

                            -- * Math
                            xor, e, exp, pi, cos_, sin_, tan_, mod_, acos_, asin_, atan_, int, log_, ln, mean, median, modes, variance, standard_deviation, subtract_headings, abs_, floor, ceiling, remainder, round, sqrt,  is_numberp,

                            -- * Misc
                            patch_size, max_pxcor, max_pycor, min_pxcor, min_pycor, world_width, world_height, clear_all_plots, clear_drawing, cd, clear_output, clear_turtles, ct, clear_patches, cp, clear_links, clear_ticks, reset_ticks, tick, tick_advance, ticks, histogram, repeat_, report, loop, stop, while, readTurtle, readPatch, readLink, stats_stm,

                            -- * Input/Output
                            show, unsafe_show, print, unsafe_print, read_from_string, --, timer, reset_timer,

                            -- * IO Operations
                            atomic, ask, askPatches, askTurtles, of_, with, snapshot, TurtlePatch (..), STMorIO (..)
 ) where

import Prelude hiding (show,print)
import qualified Prelude (show, print)
import Language.Logo.Base
import Language.Logo.Core
import Language.Logo.Conf
import Language.Logo.Exception
import Control.Concurrent.STM
import Control.Monad.Trans.Class (lift)
import qualified Control.Monad.Trans.Reader as Reader
import Control.Concurrent (threadDelay)
import qualified Control.Concurrent.Thread as Thread (forkOn, result)
import qualified Control.Concurrent.Thread.Group as ThreadG (forkOn, wait)
import Data.List
import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict as M
import Data.Array
import qualified Data.Vector as V
import Control.Applicative
import System.Random hiding (random, split)
import Data.Function
import Data.Typeable
import Control.Monad (forM_, liftM, filterM, forever, when)
import Data.Word (Word8)
import GHC.Conc (numCapabilities)
import Data.Maybe (isJust)
-- for rng
import System.CPUTime
import Data.Time.Clock ( getCurrentTime, UTCTime(..), diffUTCTime)
import Data.Ratio       ( numerator, denominator )

-- For diagrams
import qualified Diagrams.Prelude as Diag
import Diagrams.Backend.Postscript
import Data.Colour.SRGB (sRGB24)

import GHC.Conc.Sync (unsafeIOToSTM)
import Data.Functor.Identity (runIdentity)
import Data.IORef

#ifdef STATS_STM
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.Foldable as F (foldlM)
#endif


#define todo assert False undefined

{-# SPECIALIZE  self :: Agent s => C s _s' STM [s] #-}
{-# SPECIALIZE  self :: Agent s => C s _s' IO [s] #-}
-- |  Reports this turtle or patch. 
self :: (STMorIO m, Agent s) => C s _s' m [s] -- ^ returns a list (set) of agentrefs to be compatible with the 'turtle-set' function
self = do
  (s,_) <- Reader.ask
  return [s]

{-# SPECIALIZE  myself :: Agent s => C s s' STM [s'] #-}
{-# SPECIALIZE  myself :: Agent s => C s s' IO [s'] #-}
-- | "self" and "myself" are very different. "self" is simple; it means "me". "myself" means "the turtle or patch who asked me to do what I'm doing right now."
-- When an agent has been asked to run some code, using myself in that code reports the agent (turtle or patch) that did the asking. 
-- NB: Implemented for ask, of, with
myself :: (STMorIO m, Agent s) => C s s' m [s']
myself = do
  (_,m) <- Reader.ask
  return [m]

{-# SPECIALIZE  other :: Agent s => [s] -> C s _s' STM [s] #-}
{-# SPECIALIZE  other :: Agent s => [s] -> C s _s' IO [s] #-}
-- |  Reports an agentset which is the same as the input agentset but omits this agent. 
other :: (STMorIO m, Agent s) => [s] -> C s _s' m [s]
other as = do
  [s] <- self
  return $ delete s as



{-# SPECIALIZE  patches :: C _s _s' STM [Patch] #-}
{-# SPECIALIZE  patches :: C _s _s' IO [Patch] #-}
-- | Reports the agentset consisting of all patches. 
patches :: STMorIO m => C _s _s' m [Patch]
patches = return $ V.foldr' (\ v' acc -> V.toList v' ++ acc) [] __patches 

{-# SPECIALIZE  patch :: Double -> Double -> C _s _s' STM [Patch] #-}
{-# SPECIALIZE  patch :: Double -> Double -> C _s _s' IO [Patch] #-}
-- | Given the x and y coordinates of a point, reports the patch containing that point. 
patch :: STMorIO m => Double -> Double -> C _s _s' m [Patch]
patch x y = return $ if (not (horizontal_wrap_ conf) && (x' > max || x' < mix)) || (not (vertical_wrap_ conf) && (y' > may || y' < miy))
                     then nobody
                     else  [(__patches `V.unsafeIndex` (x''-mix)) `V.unsafeIndex` (y''-miy)]
           where
             mix = min_pxcor_ conf
             miy = min_pycor_ conf
             max = max_pxcor_ conf
             may = max_pycor_ conf
             x' = round x
             y' = round y
             (x'',y'') = (      -- normalize
                          ((x' + max) `mod` (max*2+1)) - max,
                          ((y' + may) `mod` (may*2+1)) - may
                         )

                           

{-# WARNING carefully "TODO" #-}
-- | Runs commands1. If a runtime error occurs inside commands1, NetLogo won't stop and alert the user that an error occurred. It will suppress the error and run commands2 instead. 
carefully :: C _s _s' STM a -> C _s _s' STM a -> C _s _s' STM a
carefully c c' = catch c (\ ex -> let _ = (ex :: SomeException) in c')

{-# SPECIALIZE  patch_at :: TurtlePatch s => Double -> Double -> C s _s' STM [Patch] #-}
{-# SPECIALIZE  patch_at :: TurtlePatch s => Double -> Double -> C s _s' IO [Patch] #-}
-- | Reports the patch at (dx, dy) from the caller, that is, the patch containing the point dx east and dy patches north of this agent. 
patch_at :: (STMorIO m, TurtlePatch s) => Double -> Double -> C s _s' m [Patch]
patch_at x y = do
  (s,_) <- Reader.ask
  (MkPatch {pxcor_ = px, pycor_=py}) <- patch_on_ s
  patch (fromIntegral px + x) (fromIntegral py +y)

{-# SPECIALIZE  patch_ahead :: Double -> C Turtle _s' STM [Patch] #-}
{-# SPECIALIZE  patch_ahead :: Double -> C Turtle _s' IO [Patch] #-}
-- | Reports the single patch that is the given distance "ahead" of this turtle, that is, along the turtle's current heading. 
patch_ahead :: STMorIO m => Double -> C Turtle _s' m [Patch]
patch_ahead n = do
  x <- xcor 
  y <- ycor
  dx_ <- dx
  dy_ <- dy
  let mx = max_pxcor_ conf
  let my = max_pycor_ conf
  let px_new = (fromIntegral (round x :: Int) :: Double) + if horizontal_wrap_ conf
                                                         then (dx_*n + fromIntegral mx) `mod_` (mx * 2 + 1) - fromIntegral mx
                                                         else dx_*n

  let py_new = (fromIntegral (round y :: Int) :: Double) + if vertical_wrap_ conf
                                                         then (dy_*n + fromIntegral my) `mod_` (my * 2 + 1) - fromIntegral my
                                                         else  dy_*n
  patch px_new py_new

{-# INLINE black #-}
black :: Double
black = 0
{-# INLINE white #-}
white :: Double
white = 9.9
{-# INLINE gray #-}
gray :: Double
gray = 5
{-# INLINE red #-}
red :: Double
red = 15
{-# INLINE orange #-}
orange :: Double
orange = 25
{-# INLINE brown #-}
brown :: Double
brown = 35
{-# INLINE yellow #-}
yellow :: Double
yellow = 45
{-# INLINE green #-}
green :: Double
green = 55
{-# INLINE lime #-}
lime :: Double
lime = 65
{-# INLINE turquoise #-}
turquoise :: Double
turquoise = 75
{-# INLINE cyan #-}
cyan :: Double
cyan = 85
{-# INLINE sky #-}
sky :: Double
sky = 95
{-# INLINE blue #-}
blue :: Double
blue = 105
{-# INLINE violet #-}
violet :: Double
violet = 115
{-# INLINE magenta #-}
magenta :: Double
magenta = 125
{-# INLINE pink #-}
pink :: Double
pink = 135

{-# SPECIALIZE  count :: Agent a => [a] -> C _s _s' STM Int #-}
{-# SPECIALIZE  count :: Agent a => [a] -> C _s _s' IO Int #-}
-- | Reports the number of agents in the given agentset. 
count :: (STMorIO m, Agent a) => [a] -> C _s _s' m Int
-- count [Nobody] = throw $ TypeException "agent" Nobody
count as = return $ length as

{-# SPECIALIZE  anyp :: Agent a => [a] -> C _s _s' STM Bool #-}
{-# SPECIALIZE  anyp :: Agent a => [a] -> C _s _s' IO Bool #-}
-- | Reports true if the given agentset is non-empty, false otherwise. 
anyp :: (STMorIO m, Agent a) => [a] -> C _s _s' m Bool
-- anyp [Nobody] = throw $ TypeException "agent" Nobody
anyp as = return $ not $ null as

allp :: (Agent a) => C a p IO Bool -> [a] -> C p p' IO Bool
allp _ [] = return True
allp r as = do
  res <- with r as
  return $ length as == length res

-- | The turtle moves forward by number units all at once (rather than one step at a time as with the forward command). 
jump :: Double -> C Turtle _s' STM ()
jump n = do
   (MkTurtle {xcor_ = tx, ycor_ = ty, heading_ = th}, _) <- Reader.ask
   x <- lift $ readTVar tx
   y <- lift $ readTVar ty
   h <- lift $ readTVar th
   let x' = x + sin_ h * n
   let y' = y + cos_ h * n
   let max_x = max_pxcor_ conf
   let dmax_x = fromIntegral max_x
   let min_x = min_pxcor_ conf
   let dmin_x = fromIntegral min_x
   let max_y = max_pycor_ conf
   let dmax_y = fromIntegral max_y
   let min_y = min_pycor_ conf
   let dmin_y = fromIntegral min_y
   if horizontal_wrap_ conf
     then
       lift $ writeTVar tx $ ((x' + dmax_x) `mod_` (max_x + abs min_x +1)) + dmin_x
     else
       when (dmin_x -0.5 < x' && x' < dmax_x + 0.5) $ lift $ writeTVar tx x'
   if vertical_wrap_ conf
     then
         lift $ writeTVar ty $ ((y' + dmax_y) `mod_` (max_y + abs min_y +1)) + dmin_y
     else
       when (dmin_y -0.5  < y' && y' < dmax_y + 0.5) $ lift $ writeTVar ty y'


-- | The turtle sets its x-coordinate to x and its y-coordinate to y. 
setxy :: Double -> Double -> C Turtle _s' STM ()
setxy x' y' = do
    (MkTurtle {xcor_ = tx, ycor_ = ty},_) <- Reader.ask
    let max_x = max_pxcor_ conf
    let dmax_x = fromIntegral max_x
    let min_x = min_pxcor_ conf
    let dmin_x = fromIntegral min_x
    let max_y = max_pycor_ conf
    let dmax_y = fromIntegral max_y
    let min_y = min_pycor_ conf
    let dmin_y = fromIntegral min_y
    if horizontal_wrap_ conf
      then
        lift $ writeTVar tx $ ((x' + dmax_x) `mod_` (max_x + abs min_x +1)) + dmin_x
      else
          if dmin_x -0.5 < x' && x' < dmax_x + 0.5
          then lift $ writeTVar tx x'
          else error "wrap"
    if vertical_wrap_ conf
      then
          lift $ writeTVar ty $ ((y' + dmax_y) `mod_` (max_y + abs min_y +1)) + dmin_y
      else
          if dmin_y -0.5  < y' && y' < dmax_y + 0.5
          then lift $ writeTVar ty y'
          else error "wrap"


-- | The turtle moves forward by number steps, one step at a time. (If number is negative, the turtle moves backward.) 
forward :: Double -> C Turtle _s' STM ()
forward 0 = return ()
forward n | n > 1 = jump 1 >> forward (n-1)
          | n < -1 = jump (-1) >> forward (n+1)
          | otherwise = jump n
 
{-# INLINE fd #-}
-- | alias for 'forward'
fd :: Double -> C Turtle _s' STM ()
fd = forward

-- | The turtle moves backward by number steps. (If number is negative, the turtle moves forward.) 
{-# INLINE back #-}
back :: Double -> C Turtle _s' STM ()
back n = forward (-n)
{-# INLINE bk #-}
-- | alias for 'back'
bk :: Double -> C Turtle _s' STM ()
bk = back

-- | As it is right now, if an agent holds a past reference to a turtle, it can still modify it and ask it to do sth. 
-- The only guarantee is that the __next__ 'turtles','turtles_at','turtles_here','turtles_on'... etc
-- will not return this dead agent.

instance TurtleLink Turtle where
    breed_ = tbreed_
    shape_ = tshape_
    label_ = tlabel_
    label_color_ = tlabel_color_
    color_ = tcolor_
    die = do
      (MkTurtle {who_ = tw},_) <- Reader.ask
      lift $ modifyTVar' __turtles (IM.delete tw)

instance TurtleLink Link where
    breed_ = lbreed_
    shape_ = lshape_
    label_ = llabel_
    label_color_ = llabel_color_
    color_ = lcolor_
    die = do
      (MkLink {end1_ = e1, end2_ = e2, directed_ = d}, _) <- Reader.ask
      lift $ modifyTVar' __links (M.delete (e1,e2) . 
                                           (if d -- is directed
                                            then id
                                            else M.delete (e2,e1)
                                           ))
class Agent s => TurtlePatch s where
    patch_on_ :: STMorIO m => s -> C s _s' m Patch

instance TurtlePatch Patch where
    patch_on_ = return

instance TurtlePatch Turtle where
    patch_on_ (MkTurtle {xcor_=tx,ycor_=ty}) = do
                 x <- lift $ readTVarSI tx
                 y <- lift $ readTVarSI ty
                 [p] <- patch x y
                 return p

{-# SPECIALIZE  patch_on_ :: TurtlePatch s => s -> C s _s' IO Patch #-}
{-# SPECIALIZE  patch_on_ :: TurtlePatch s => s -> C s _s' STM Patch #-}

{-# SPECIALIZE  turtle_set :: [C _s _s' STM [Turtle]] -> C _s _s' STM [Turtle] #-}
{-# SPECIALIZE  turtle_set :: [C _s _s' IO [Turtle]] -> C _s _s' IO [Turtle] #-}
-- | Reports an agentset containing all of the turtles anywhere in any of the inputs.
-- | NB: HLogo no support for nested turtle_set concatenation/flattening
turtle_set :: STMorIO m => [C _s _s' m [Turtle]] -> C _s _s' m [Turtle]
turtle_set ts = liftM (foldr (\ x acc -> 
                                  -- if x == Nobody -- filter Nobody
                                  -- then acc
                                  -- else case x of -- type check
                                  --        TurtleRef _ _ -> 
                              if x `elem` acc -- nub
                              then acc
                              else x:acc
                                         -- _ -> throw $ TypeException "turtle" x
                             ) [] . concat) (sequence ts)

{-# SPECIALIZE  patch_set :: [C _s _s' STM [Patch]] -> C _s _s' STM [Patch] #-}
{-# SPECIALIZE  patch_set :: [C _s _s' IO [Patch]] -> C _s _s' IO [Patch] #-}
-- | Reports an agentset containing all of the patches anywhere in any of the inputs.
-- | NB: HLogo no support for nested patch_set concatenation/flattening
patch_set :: STMorIO m => [C _s _s' m [Patch]] -> C _s _s' m [Patch]
patch_set ts = liftM (foldr (\ x acc -> 
                                 -- if x == Nobody -- filter Nobody
                                 -- then acc
                                 -- else case x of -- type check
                                 --        PatchRef _ _ -> 
                             if x `elem` acc -- nub
                             then acc
                             else x:acc
                                        -- _ -> throw $ TypeException "patch" x
                            ) [] . concat) (sequence ts)

{-# SPECIALIZE  link_set :: [C _s _s' STM [Link]] -> C _s _s' STM [Link] #-}
{-# SPECIALIZE  link_set :: [C _s _s' IO [Link]] -> C _s _s' IO [Link] #-}
-- | Reports an agentset containing all of the links anywhere in any of the inputs.
-- | NB: HLogo no support for nested turtle_set concatenation/flattening
link_set :: STMorIO m => [C _s _s' m [Link]] -> C _s _s' m [Link]
link_set ts = liftM (foldr (\ x acc -> -- if x == Nobody -- filter Nobody
                                      -- then acc
                                      -- else case x of -- type check
                                      --        LinkRef _ _ -> 
                            if x `elem` acc -- nub
                            then acc
                            else x:acc
                                             -- _ -> throw $ TypeException "link" x
                           ) [] . concat) (sequence ts)


-- {-# SPECIALIZE can_movep :: Double -> CSTM Bool #-}
-- {-# SPECIALIZE can_movep :: Double -> CIO Bool #-}
-- | Reports true if this turtle can move distance in the direction it is facing without violating the topology; reports false otherwise. 

-- can_movep :: STMorIO m => Double -> C Turtle _s' m Bool
-- can_movep n = liftM ( /= [Nobody]) $ patch_ahead n


set_heading :: Double -> C Turtle _s' STM ()
set_heading v = do
  (t,_) <- Reader.ask
  lift $ writeTVar (heading_ t) v

{-# SPECIALIZE  pxcor :: TurtlePatch s => C s _s' STM Int #-}
{-# SPECIALIZE  pxcor :: TurtlePatch s => C s _s' IO Int #-}
-- |These are built-in patch variables. They hold the x and y coordinate of the patch. They are always integers. You cannot set these variables, because patches don't move 
pxcor :: (TurtlePatch s, STMorIO m) => C s _s' m Int
pxcor = do
  (s,_) <- Reader.ask
  (MkPatch {pxcor_ = x}) <- patch_on_ s
  return x

{-# SPECIALIZE  pycor :: TurtlePatch s => C s _s' STM Int #-}
{-# SPECIALIZE  pycor :: TurtlePatch s => C s _s' IO Int #-}
-- | These are built-in patch variables. They hold the x and y coordinate of the patch. They are always integers. You cannot set these variables, because patches don't mov 
pycor :: (TurtlePatch s, STMorIO m) => C s _s' m Int
pycor = do
  (s,_) <- Reader.ask
  (MkPatch {pycor_ = y}) <- patch_on_ s
  return y

{-# SPECIALIZE  set_plabel :: String -> C Turtle _s' STM () #-}
{-# SPECIALIZE  set_plabel :: String -> C Patch _s' STM () #-}
set_plabel :: TurtlePatch s => String -> C s _s' STM ()
set_plabel l = do
  (s,_) <- Reader.ask
  (MkPatch {plabel_ = tl}) <- patch_on_ s
  lift $ writeTVar tl l

{-# SPECIALIZE  set_pcolor :: Double -> C Turtle _s' STM () #-}
{-# SPECIALIZE  set_pcolor :: Double -> C Patch _s' STM () #-}
set_pcolor :: TurtlePatch s => Double -> C s _s' STM ()
set_pcolor c = do
  (s,_) <- Reader.ask
  (MkPatch {pcolor_ = tc}) <- patch_on_ s
  lift $ writeTVar tc c

{-# SPECIALIZE  set_breed :: String -> C Turtle _s' STM () #-}
{-# SPECIALIZE  set_breed :: String -> C Link _s' STM () #-}
set_breed :: TurtleLink s => String -> C s _s' STM ()
set_breed v = do
  (s,_) <- Reader.ask
  lift $ writeTVar (breed_ s) v

{-# SPECIALIZE  set_color :: Double -> C Turtle _s' STM () #-}
{-# SPECIALIZE  set_color :: Double -> C Link _s' STM () #-}
set_color :: TurtleLink s => Double -> C s _s' STM ()
set_color v = do
  (s,_) <- Reader.ask
  lift $ writeTVar (color_ s) v

{-# SPECIALIZE  set_label_color :: Double -> C Turtle _s' STM () #-}
{-# SPECIALIZE   set_label_color :: Double -> C Link _s' STM () #-}
set_label_color :: TurtleLink s => Double -> C s _s' STM ()
set_label_color v = do
  (s,_) <- Reader.ask
  lift $ writeTVar (label_color_ s) v

set_xcor :: Double -> C Turtle _s' STM ()
set_xcor x' = do
    (MkTurtle {xcor_ = tx},_) <- Reader.ask
    let max_x = max_pxcor_ conf
    let dmax_x = fromIntegral max_x
    let min_x = min_pxcor_ conf
    let dmin_x = fromIntegral min_x
    if horizontal_wrap_ conf
     then
         lift $ writeTVar tx $ ((x' + dmax_x) `mod_` (max_x + abs min_x +1)) + dmin_x
     else
         if dmin_x -0.5 < x' && x' < dmax_x + 0.5
         then lift $ writeTVar tx x'
         else error "wrap"


set_size :: Double -> C Turtle _s' STM ()
set_size v = do
  (t,_) <- Reader.ask
  lift $ writeTVar (size_ t) v

set_ycor :: Double -> C Turtle _s' STM ()
set_ycor y' = do
   (MkTurtle {ycor_ = ty},_) <- Reader.ask
   let max_y = max_pycor_ conf
   let dmax_y = fromIntegral max_y
   let min_y = min_pycor_ conf
   let dmin_y = fromIntegral min_y
   if vertical_wrap_ conf
     then
         lift $ writeTVar ty $ ((y' + dmax_y) `mod_` (max_y + abs min_y +1)) + dmin_y
     else
         if dmin_y -0.5 < y' && y' < dmax_y + 0.5
         then lift $ writeTVar ty y'
         else error "wrap"
 
{-# SPECIALIZE  who :: C Turtle _s' STM Int #-}
{-# SPECIALIZE  who :: C Turtle _s' IO Int #-}
-- | This is a built-in turtle variable. It holds the turtle's "who number" or ID number, an integer greater than or equal to zero. You cannot set this variable; a turtle's who number never changes. 
who :: STMorIO m => C Turtle _s' m Int
who = do
  (MkTurtle {who_=tw},_) <- Reader.ask
  return tw


{-# SPECIALIZE  dx :: C Turtle _s' STM Double #-}
{-# SPECIALIZE  dx :: C Turtle _s' IO Double #-}
-- | Reports the x-increment (the amount by which the turtle's xcor would change) if the turtle were to take one step forward in its current heading. 
dx :: STMorIO m => C Turtle _s' m Double
dx = liftM sin_ heading

{-# SPECIALIZE  dy :: C Turtle _s' STM Double #-}
{-# SPECIALIZE  dy :: C Turtle _s' IO Double #-}
-- | Reports the y-increment (the amount by which the turtle's ycor would change) if the turtle were to take one step forward in its current heading. 
dy :: STMorIO m => C Turtle _s' m Double
dy = liftM cos_ heading

-- | Reports a number suitable for seeding the random number generator.
-- The numbers reported by new-seed are based on the current date and time in milliseconds. 
-- Unlike NetLogo's new-seed, HLogo may report the same number twice in succession.
--
-- NB: taken from Haskell's random library
new_seed :: C _s _s' STM Int
new_seed = do
    cpt          <- lift $ unsafeIOToSTM getCPUTime
    (sec, psec) <- lift $ unsafeIOToSTM getTime
    return $ fromIntegral (sec * 12345 + psec + cpt)
        where
          getTime :: IO (Integer, Integer)
          getTime = do
             utc <- getCurrentTime
             let daytime = toRational $ utctDayTime utc
             return $ quotRem (numerator daytime) (denominator daytime)

random_seed :: Player s => Int -> C s _s' STM ()
random_seed i = do
  (s,_) <- Reader.ask
  let g = gen_ s
  lift $ writeTVar g (mkStdGen i)
                
-- | Reports a random floating point number from the allowable range of turtle coordinates along the given axis, x . 
random_xcor :: Player s => C s _s' STM Double
random_xcor = do
  (s,_) <- Reader.ask
  let g = gen_ s
  gen <- lift $ readTVar g
  let (v, gen') = randomR (fromIntegral (min_pxcor_ conf) :: Double, fromIntegral $ max_pxcor_ conf) gen
  lift $ writeTVar g gen'
  return v

-- | Reports a random floating point number from the allowable range of turtle coordinates along the given axis, y. 
random_ycor :: Player s => C s _s' STM Double
random_ycor = do
  (s,_) <- Reader.ask
  let g = gen_ s
  gen <- lift $ readTVar g
  let (v, gen') = randomR (fromIntegral (min_pycor_ conf) :: Double, fromIntegral $ max_pycor_ conf) gen
  lift $ writeTVar g gen'
  return v

-- | Reports a random integer ranging from min-pxcor to max-pxcor inclusive. 
random_pxcor :: Player s => C s _s' STM Int
random_pxcor = do
  (s,_) <- Reader.ask
  let g = gen_ s
  gen <- lift $ readTVar g
  let (v, gen') = randomR (min_pxcor_ conf, max_pxcor_ conf) gen
  lift $ writeTVar g gen'
  return v

-- | Reports a random integer ranging from min-pycor to max-pycor inclusive. 
random_pycor :: Player s => C s _s' STM Int
random_pycor = do
  (s,_) <- Reader.ask
  let g = gen_ s
  gen <- lift $ readTVar g
  let (v, gen') = randomR (min_pycor_ conf, max_pycor_ conf) gen
  lift $ writeTVar g gen'
  return v

{-# WARNING random "maybe it can become faster with some small fraction added to the input or subtracted and then floored" #-}
{-# SPECIALIZE random :: (Num b, Real a) => a -> C Observer () STM b #-}
{-# SPECIALIZE random :: (Num b, Real a) => a -> C Turtle _s' STM b #-}
{-# SPECIALIZE random :: (Num b, Real a) => a -> C Patch _s' STM b #-}
{-# SPECIALIZE random :: (Num b, Real a) => a -> C Link _s' STM b #-}
-- | If number is positive, reports a random integer greater than or equal to 0, but strictly less than number.
-- If number is negative, reports a random integer less than or equal to 0, but strictly greater than number.
-- If number is zero, the result is always 0 as well. 
random :: (Player s, Num b, Real a) => a -> C s _s' STM b
random x = do
  (s,_) <- Reader.ask
  let ts = gen_ s
  gen <- lift $ readTVar ts
  let (n, f) = properFraction (realToFrac x)
  let randRange = if n > 0 
                  then (0, if f == 0 
                       then n-1
                       else n)
                  else (if f == 0
                        then n+1
                        else n, 0)
  let (v, gen') = randomR randRange gen :: (Int, StdGen)
  lift $ writeTVar ts gen'
  return (fromIntegral v)

-- |  If number is positive, reports a random floating point number greater than or equal to 0 but strictly less than number.
-- If number is negative, reports a random floating point number less than or equal to 0, but strictly greater than number.
-- If number is zero, the result is always 0. 
random_float :: Player s => Double -> C s _s' STM Double
random_float x = do
  (s,_) <- Reader.ask
  let ts = gen_ s
  gen <- lift $ readTVar ts
  let (v, gen') = randomR (if x > 0 then (0,x) else (x,0)) gen
  lift $ writeTVar ts gen'
  return v

{-# WARNING random_exponential "TODO" #-}
-- | random-exponential reports an exponentially distributed random floating point number. 
random_exponential :: t -> t1
random_exponential _m = todo

{-# WARNING random_gamma "TODO" #-}
-- | random-gamma reports a gamma-distributed random floating point number as controlled by the floating point alpha and lambda parameters. 
random_gamma :: t -> t1 -> t2
random_gamma _a _l = todo

{-# WARNING random_normal "TODO" #-}
-- | random-normal reports a normally distributed random floating point number. 
random_normal :: t -> t1 -> t2
random_normal _m _s = todo

{-# WARNING random_poisson "TODO" #-}
-- | random-poisson reports a Poisson-distributed random integer. 
random_poisson :: t -> t1
random_poisson _m = todo


-- | This turtle moves to the origin (0,0). Equivalent to setxy 0 0. 
{-# INLINE home #-}
home :: C Turtle _s' STM ()
home = setxy 0 0

-- | The turtle turns right by number degrees. (If number is negative, it turns left.) 
right :: Double -> C Turtle _s' STM ()
right n = do
  (t,_) <- Reader.ask
  lift $ modifyTVar' (heading_ t) (\ h -> mod_ (h+n) 360)
{-# INLINE rt #-}
-- | alias for 'right'
rt :: Double -> C Turtle _s' STM ()
rt = right

-- | The turtle turns left by number degrees. (If number is negative, it turns right.) 
{-# INLINE left #-}
left :: Double -> C Turtle _s' STM ()
left n = right (-n)

{-# INLINE lt #-}
-- | alias for 'left'
lt :: Double -> C Turtle _s' STM ()
lt = left

{-# WARNING delta "TODO: there is some problem here, an argument is ignored" #-}
-- | Internal
delta :: (Num a, Ord a) => a -> a -> t -> a
delta a1 a2 _aboundary =
    min (abs (a2 - a1)) (abs (a2 + a1) + 1)


-- | This is a special value which some primitives such as turtle, one-of, max-one-of, etc. report to indicate that no agent was found. Also, when a turtle dies, it becomes equal to nobody. 
--
-- It can be returned from all primitives that normally return 1 agent. It can also be returned from a turtle reference that got died or the 'turtle' primitive to a dead agent,, like implicitly nullifying the agent.
nobody :: Agent a => [a]
nobody = error "nobody"

{-# WARNING downhill "TODO" #-}
-- | Moves the turtle to the neighboring patch with the lowest value for patch-variable. 
-- If no neighboring patch has a smaller value than the current patch, the turtle stays put. 
-- If there are multiple patches with the same lowest value, the turtle picks one randomly. 
-- Non-numeric values are ignored.
-- downhill considers the eight neighboring patches
downhill :: t
downhill = todo

{-# WARNING downhill4 "TODO" #-}
-- | Moves the turtle to the neighboring patch with the lowest value for patch-variable. 
-- If no neighboring patch has a smaller value than the current patch, the turtle stays put. 
-- If there are multiple patches with the same lowest value, the turtle picks one randomly. 
-- Non-numeric values are ignored.
-- downhill4 only considers the four neighbors. 
downhill4 :: t
downhill4 = todo

-- | Set the caller's heading towards agent. 
{-# INLINE face #-}
face :: TurtlePatch a => [a] -> C Turtle _s' STM ()
face a = set_heading =<< towards a

{-# WARNING towardsxy "TODO" #-}
-- | Reports the heading from the turtle or patch towards the point (x,y). 
towardsxy :: t
towardsxy = todo


-- | The turtle makes itself invisible. 
hide_turtle :: C Turtle _s' STM ()
hide_turtle = do
  (MkTurtle {hiddenp_ = th},_) <- Reader.ask
  lift $ writeTVar th True

{-# INLINE ht #-}
-- | alias for 'hide_turtle'
ht :: C Turtle _s' STM ()
ht = hide_turtle

-- | The turtle becomes visible again. 
show_turtle :: C Turtle _s' STM ()
show_turtle = do
  (MkTurtle {hiddenp_ = th},_) <- Reader.ask
  lift $ writeTVar th False

{-# INLINE st #-}
-- | alias for 'show_turtle'
st :: C Turtle _s' STM ()
st = show_turtle

-- | The turtle changes modes between drawing lines, removing lines or neither. 
pen_down :: C Turtle _s' STM ()
pen_down = do
  (MkTurtle {pen_mode_ = tp},_) <- Reader.ask
  lift $ writeTVar tp Down

{-# INLINE pd #-}
-- | alias for 'pen_down'
pd :: C Turtle _s' STM ()
pd = pen_down

-- | The turtle changes modes between drawing lines, removing lines or neither. 
pen_up :: C Turtle _s' STM ()
pen_up = do
  (MkTurtle {pen_mode_ = tp},_) <- Reader.ask
  lift $ writeTVar tp Up

{-# INLINE pu #-}
-- | alias for 'pen_up'
pu :: C Turtle _s' STM ()
pu = pen_up

pen_erase :: C Turtle _s' STM ()
-- | The turtle changes modes between drawing lines, removing lines or neither. 
pen_erase = do
  (MkTurtle {pen_mode_ = tp},_) <- Reader.ask
  lift $ writeTVar tp Erase

{-# INLINE pe #-}
-- | alias for 'pen_erase'
pe :: C Turtle _s' STM ()
pe = pen_erase







-- | This reporter lets you give a turtle a "cone of vision" in front of itself. 
in_cone :: t
in_cone = todo




{-# SPECIALIZE  no_turtles :: C _s _s' STM [Turtle] #-}
{-# SPECIALIZE  no_turtles :: C _s _s' IO [Turtle] #-}
-- | Reports an empty turtle agentset. 
no_turtles :: STMorIO m => C _s _s' m [Turtle]
no_turtles = return []

{-# SPECIALIZE  no_patches :: C _s _s' STM [Patch] #-}
{-# SPECIALIZE  no_patches :: C _s _s' IO [Patch] #-}
-- | Reports an empty patch agentset. 
no_patches :: STMorIO m => C _s _s' m [Patch]
no_patches = return []

{-# SPECIALIZE  no_links :: C _s _s' STM [Link] #-}
{-# SPECIALIZE  no_links :: C _s _s' IO [Link] #-}
-- | Reports an empty link agentset. 
no_links :: STMorIO m => C _s _s' m [Link]
no_links = return []


-- | Reports true if either boolean1 or boolean2 is true, but not when both are true. 
xor :: Bool -> Bool -> Bool
xor p q = (p || q) && not (p && q)


{-# SPECIALIZE  patch_size :: C _s _s' STM Int #-}
{-# SPECIALIZE  patch_size :: C _s _s' IO Int #-}
patch_size :: STMorIO m => C _s _s' m Int
patch_size = return $ patch_size_ conf

{-# SPECIALIZE  max_pxcor :: C _s _s' STM Int #-}
{-# SPECIALIZE  max_pxcor :: C _s _s' IO Int #-}
-- | This reporter gives the maximum x-coordinate for patches, which determines the size of the world. 
max_pxcor :: STMorIO m => C _s _s' m Int
max_pxcor = return $ max_pxcor_ conf

{-# SPECIALIZE  max_pycor :: C _s _s' STM Int #-}
{-# SPECIALIZE  max_pycor :: C _s _s' IO Int #-}
-- | This reporter gives the maximum y-coordinate for patches, which determines the size of the world. 
max_pycor :: STMorIO m => C _s _s' m Int
max_pycor = return $ max_pycor_ conf

{-# SPECIALIZE  min_pxcor :: C _s _s' STM Int #-}
{-# SPECIALIZE  min_pxcor :: C _s _s' IO Int #-}
-- | This reporter gives the minimum x-coordinate for patches, which determines the size of the world. 
min_pxcor :: STMorIO m => C _s _s' m Int
min_pxcor = return $ min_pxcor_ conf

{-# SPECIALIZE  min_pycor :: C _s _s' STM Int #-}
{-# SPECIALIZE  min_pycor :: C _s _s' IO Int #-}
-- | This reporter gives the maximum y-coordinate for patches, which determines the size of the world. 
min_pycor :: STMorIO m => C _s _s' m Int
min_pycor = return $ min_pycor_ conf

{-# SPECIALIZE  world_width :: C _s _s' STM Int #-}
{-# SPECIALIZE  world_width :: C _s _s' IO Int #-}
-- | This reporter gives the total width of the NetLogo world. 
world_width :: STMorIO m => C _s _s' m Int
world_width = return $ max_pxcor_ conf - min_pxcor_ conf + 1

{-# SPECIALIZE  world_height :: C _s _s' STM Int #-}
{-# SPECIALIZE  world_height :: C _s _s' IO Int #-}
-- | This reporter gives the total height of the NetLogo world. 
world_height :: STMorIO m => C _s _s' m Int
world_height = return $ max_pycor_ conf - min_pycor_ conf + 1


{-# WARNING clear_all_plots "TODO" #-}
-- | Clears every plot in the model.
clear_all_plots :: C Observer () IO ()
clear_all_plots = todo

{-# WARNING clear_drawing "TODO" #-}
-- | Clears all lines and stamps drawn by turtles. 
clear_drawing :: C Observer () IO ()
clear_drawing = todo

{-# INLINE cd #-}
-- | alias for 'clear_drawing'
cd :: C Observer () IO ()
cd = clear_drawing

{-# WARNING clear_output "TODO" #-}
-- | Clears all text from the model's output area, if it has one. Otherwise does nothing. 
clear_output :: C Observer () IO ()
clear_output = todo


-- | Kills all turtles.
-- Also resets the who numbering, so the next turtle created will be turtle 0.
clear_turtles :: C Observer () IO ()
clear_turtles = atomic $ lift $ do
                  writeTVar __turtles IM.empty
                  writeTVar __who 0

{-# INLINE ct #-}
-- | alias for 'clear_turtles'
ct :: C Observer () IO ()
ct = clear_turtles

{-# INLINE clear_links #-}
-- | Kills all links.
clear_links :: C Observer () IO ()
clear_links = atomic $ lift $ writeTVar __links M.empty

-- | Clears the patches by resetting all patch variables to their default initial values, including setting their color to black. 
clear_patches :: C Observer () IO ()
clear_patches = V.mapM_ (V.mapM_ (\ (MkPatch {pcolor_=pc, plabel_=pl, plabel_color_=plc, pvars_=po, pgen_=pg})  -> do
                            atomic $ lift $ writeTVar pc 0
                            atomic $ lift $ writeTVar pl ""
                            atomic $ lift $ writeTVar plc 9.9
                            atomic $ lift $ mapM_ (`writeTVar` 0) (elems po) -- patches-own to 0
                            atomic $ lift $ writeTVar pg $ mkStdGen 3
                           )) __patches

{-# INLINE cp #-}
-- | alias for 'clear_patches'
cp :: C Observer () IO ()
cp = clear_patches


{-# INLINE clear_ticks #-}
-- | Clears the tick counter.
-- Does not set the counter to zero. After this command runs, the tick counter has no value. Attempting to access or update it is an error until reset-ticks is called. 
clear_ticks :: C Observer () IO ()
clear_ticks = lift $ atomically $ writeTVar __tick (error "The tick counter has not been started yet. Use RESET-TICKS.")

{-# INLINE reset_ticks #-}
-- | Resets the tick counter to zero, sets up all plots, then updates all plots (so that the initial state of the world is plotted). 
reset_ticks :: C Observer () IO ()
reset_ticks = lift $ atomically $ writeTVar __tick 0

{-# INLINE tick #-}
-- | Advances the tick counter by one and updates all plots. 
tick :: C Observer () IO ()
tick = tick_advance 1

{-# WARNING tick_advance "TODO: dynamic typing, float" #-}
{-# INLINE tick_advance #-}
-- | Advances the tick counter by number. The input may be an integer or a floating point number. (Some models divide ticks more finely than by ones.) The input may not be negative. 
tick_advance :: Double -> C Observer () IO ()
tick_advance n = lift $ atomically $ modifyTVar' __tick (+n)

{-# INLINE butfirst #-}
-- | When used on a list, but-first reports all of the list items of list except the first
butfirst :: [a] -> [a]
butfirst = tail

{-# INLINE butlast #-}
-- | but-last reports all of the list items of list except the last. 
butlast :: [a] -> [a]
butlast = init

{-# INLINE emptyp #-}
-- | Reports true if the given list or string is empty, false otherwise. 
emptyp :: [a] -> Bool
emptyp = null

{-# INLINE first #-}
-- | On a list, reports the first (0th) item in the list. 
first :: [a] -> a
first = head

{-# INLINE foreach #-}
-- | With a single list, runs the task for each item of list. 
foreach :: STMorIO m => [a] -> (a -> C _s _s' m b) -> C _s _s' m ()
foreach = forM_

{-# INLINE fput #-}
-- | Adds item to the beginning of a list and reports the new list. 
fput :: a -> [a] -> [a]
fput = (:)

{-# WARNING histogram "TODO" #-}
-- | Histograms the values in the given list
-- Draws a histogram showing the frequency distribution of the values in the list. The heights of the bars in the histogram represent the numbers of values in each subrange. 
histogram :: t
histogram = todo

-- | Runs commands number times. 
repeat_ :: STMorIO m => Int -> C _s _s' m a -> C _s _s' m ()
repeat_ 0 _ = return ()
repeat_ n c = c >> repeat_ (n-1) c

{-# INLINE report #-}
-- | Immediately exits from the current to-report procedure and reports value as the result of that procedure. report and to-report are always used in conjunction with each other. 
-- | NB: IN HLogo, It does not exit the procedure, but it will if the report primitive happens to be the last statement called from the procedure
report :: STMorIO m => a -> C _s _s' m a
report = return


{-# INLINE item #-}
-- | On lists, reports the value of the item in the given list with the given index. 
item :: Int -> [a] -> a
item i l = l !! i

{-# INLINE list #-}
-- | Reports a list containing the given items.
list :: t -> t -> [t]
list x y = [x,y]

{-# INLINE lput #-}
-- | Adds value to the end of a list and reports the new list. 
lput :: a -> [a] -> [a]
lput x l = l ++ [x]

{-# INLINE memberp #-}
-- | For a list, reports true if the given value appears in the given list, otherwise reports false. 
memberp :: Eq a => a -> [a] -> Bool
memberp = elem


-- | Reports a list of length size containing values computed by repeatedly running the task. 
n_values :: (Eq a, STMorIO m, Num a) => a -> (a -> C _s _s' m t) -> C _s _s' m [t]
n_values 0 _ = return []
n_values s f = do
    h <- f s 
    t <- n_values (s-1) f
    return (h:t)

{-# WARNING position "TODO: requires dynamic typing" #-}
{-# INLINE position #-}
-- | On a list, reports the first position of item in list, or false if it does not appear. 
position :: (a -> Bool) -> [a] -> Maybe a
position = find

-- |  From an agentset, reports a random agent. If the agentset is empty, reports nobody.
-- From a list, reports a random list item. It is an error for the list to be empty. 
one_of :: Player s => [a] -> C s _s' STM [a]
one_of [] = error "empty list"
one_of l = do
  (s,_) <- Reader.ask
  let ts = gen_ s
  gen <- lift $ readTVar ts
  let (v,gen') = randomR (0, length l -1) gen
  lift $ writeTVar ts gen'
  return [l !! v]

-- Uses instead agent_one_of when types match
-- {-# RULES "one_of/AgentRef" one_of = agent_one_of #-}
agent_one_of :: (Player s, Agent a) => [a] -> C s _s' STM [a]
agent_one_of [] = return nobody
agent_one_of l = do
  (s,_) <- Reader.ask
  let ts = gen_ s
  gen <- lift $ readTVar ts
  let (v,gen') = randomR (0, length l -1) gen
  lift $ writeTVar ts gen'
  return [l !! v]

-- |  From an agentset, reports an agentset of size size randomly chosen from the input set, with no repeats.
-- From a list, reports a list of size size randomly chosen from the input set, with no repeats. 
n_of :: (Player s, Eq a) => Int -> [a] -> C s _s' STM [a]
n_of n ls | n == 0     = return []
          | n < 0     = error "negative index"
          | otherwise = do
  [o] <- one_of ls
  ns <- n_of (n-1) (delete o ls)
  return (o:ns)

-- Uses instead agent_one_of when types match
-- {-# RULES "n_of/AgentRef" n_of = agent_n_of #-}
agent_n_of :: (Player s, Agent a) => Int -> [a] -> C s _s' STM [a]
agent_n_of n ls | n == 0     = return []
                | n < 0     = error "negative index"
                | otherwise = do
  [o] <- one_of ls
  -- when (o == Nobody) $ error "empty agentset"
  ns <- n_of (n-1) (delete o ls)
  return (o:ns)


{-# WARNING min_one_of "TODO: currently deterministic and no randomness on tie breaking" #-}
-- | Reports a random agent in the agentset that reports the lowest value for the given reporter. If there is a tie, this command reports one random agent that meets the condition.
--min_one_of :: Ord a => [AgentRef] -> CIO a -> CIO [AgentRef]
min_one_of as r = do
  rs <- of_ r as
  return [snd $ minimum $ zip rs as]

{-# WARNING max_one_of "TODO: currently deterministic and no randomness on tie breaking" #-}
-- | Reports the agent in the agentset that has the highest value for the given reporter. If there is a tie this command reports one random agent with the highest value. If you want all such agents, use with-max instead. 
--max_one_of :: Ord a => [AgentRef] -> CIO a -> CIO [AgentRef]
max_one_of as r = do
  rs <- of_ r as
  return [snd $ maximum $ zip rs as]



{-# INLINE reduce #-}
-- | Reduces a list from left to right using the given task, resulting in a single value. (foldl)
reduce :: (b -> a -> b) -> b -> [a] -> b
reduce = foldl

{-# WARNING remove "TODO" #-}
-- | For a list, reports a copy of list with all instances of item removed. 
remove :: t
remove = todo

{-# INLINE remove_duplicates #-}
-- | Reports a copy of list with all duplicate items removed. The first of each item remains in place. 
remove_duplicates :: (STMorIO m, Eq a) => [a] -> C _s _s' m [a]
remove_duplicates = return . nub

{-# WARNING remove_item "TODO" #-}
-- | For a list, reports a copy of list with the item at the given index removed. 
remove_item :: t
remove_item = todo

{-# WARNING replace_item "TODO" #-}
-- | On a list, replaces an item in that list. index is the index of the item to be replaced, starting with 0. 
replace_item :: t
replace_item = todo

{-# WARNING sentence "TODO: requires dynamic_typing" #-}
{-# INLINE sentence #-}
-- | Makes a list out of the values. 
sentence :: [a] -> [a] -> [a]
sentence = (++)

{-# WARNING shuffle "TODO: make it tail-recursive, optimize with arrays <http://www.haskell.org/haskellwiki/Random_shuffle>" #-}
-- | Reports a new list containing the same items as the input list, but in randomized order. 
shuffle :: (Player s, Eq a) => [a] -> C s _s' STM [a]
shuffle [] = return []
shuffle [x] = return [x]
shuffle l = do 
  [x] <- one_of l
  xs <- shuffle (delete x l)
  return $ x:xs

{-# INLINE sort_ #-}
-- | Reports a sorted list of numbers, strings, or agents. 
sort_ :: (STMorIO m, Ord a) => [a] -> C _s _s' m [a]
sort_ = return . sort

{-# WARNING sort_by "TODO: requires dynamic_typing" #-}
{-# INLINE sort_by #-}
-- | If the input is a list, reports a new list containing the same items as the input list, in a sorted order defined by the boolean reporter task. 
sort_by :: STMorIO m => (a -> a -> Ordering) -> [a] -> C _s _s' m [a]
sort_by c l = return $ sortBy c l

-- | Reports a list of agents, sorted according to each agent's value for reporter. Ties are broken randomly. 
-- sort_on :: Ord a => CSTM a -> [AgentRef] -> CSTM [AgentRef]
sort_on rep as = do
  (s,_) <- Reader.ask
  xs <- lift . sequence $ [Reader.runReaderT rep (a,s) | a <- as]
  let rs = zip xs as
  return $ map snd $ sortBy (compare `on` fst) rs where


-- | Reports just a section of the given list or string, ranging between the first position (inclusive) and the second position (exclusive). 
-- 0-indexed
sublist :: [a] -> Int -> Int -> [a]
sublist l x y = take (y-x) . drop x $ l
{-# INLINE substring #-}
-- | Reports just a section of the given list or string, ranging between the first position (inclusive) and the second position (exclusive). 
substring :: [a] -> Int -> Int -> [a]
substring = sublist

{-# INLINE read_from_string #-}
-- | Interprets the given string as if it had been typed in the Command Center, and reports the resulting value.
read_from_string :: Read a => String -> a
read_from_string = read

{-# INLINE word #-}
-- | Concatenates the inputs together and reports the result as a string.
word :: Show a => [a] -> String
word = concatMap Prelude.show

{-# INLINE abs_ #-}
-- | Reports the absolute value of number. 
abs_ :: (STMorIO m, Num a) => a -> C _s _s' m a
abs_ = return . abs

{-# INLINE e #-}
-- | Mathematical Constant
e :: Double
e = exp 1

-- | Reports the cosine of the given angle. Assumes the angle is given in degrees. 
cos_ :: Double -> Double
cos_ = cos . toRadians

-- | Reports the sine of the given angle. Assumes the angle is given in degrees. 
sin_ :: Double -> Double
sin_ = sin . toRadians

-- | Reports the tangent of the given angle. 
tan_ :: Double -> Double
tan_ = tan . toRadians

-- | Internal
toRadians :: Floating a => a -> a
toRadians deg = deg * pi / 180

-- | Internal
toDegrees :: Floating a => a -> a
toDegrees rad = rad * 180 / pi

-- | Reports number1 modulo number2
mod_ :: Double -> Int -> Double
x `mod_` y | x == 0 = 0
           | otherwise =  fromIntegral (x' `mod` y) + (x - fromIntegral x')
           where x' = floor x

-- | Reports the arc cosine (inverse cosine) of the given number. 
acos_ :: Double -> Double
acos_ = toDegrees . acos

-- | Reports the arc sine (inverse sine) of the given number. 
asin_ :: Double -> Double
asin_ = toDegrees . asin

-- | Reports the arc tangent (inverse tangent) of the given number. 
atan_ :: RealFloat r => r -> r -> r
atan_ x y = toDegrees $ atan2 (toRadians x) (toRadians y)

{-# INLINE int #-}
-- | Reports the integer part of number -- any fractional part is discarded. 
int :: (Integral b, RealFrac a) => a -> b
int = truncate

{-# INLINE ln #-}
-- | Reports the natural logarithm of number, that is, the logarithm to the base e (2.71828...). 
ln :: Floating a => a -> a
ln = log

{-# INLINE log_ #-}
-- | Reports the logarithm of number in base base. 
log_ :: Double -> Double -> Double
log_ = flip logBase


{-# INLINE max_ #-}
-- | Reports the maximum number value in the list. It ignores other types of items. 
max_ :: Ord a => [a] -> a
max_ = maximum

{-# INLINE min_ #-}
-- Reports the minimum number value in the list. It ignores other types of items. 
min_ :: Ord a => [a] -> a
min_ = minimum

-- | Reports the statistical mean of the numeric items in the given list. Ignores non-numeric items.
mean :: Real a => [a] -> Double
mean l = let (t,n) = foldl' (\(b,c) a -> (a+b,c+1)) (0,0) l 
         in realToFrac t / realToFrac n

-- | Reports the statistical median of the numeric items of the given list. Ignores non-numeric items.
median :: [Double] -> Double
median l = let (d, m) = length l `divMod` 2
           in case m of
                1 -> l !! d
                0 -> (l !! d + l !! (d-1)) / 2
                _ -> throw DevException

{-# WARNING modes "TODO" #-}
-- | Reports a list of the most common item or items in list. 
modes :: t
modes = todo

{-# INLINE remainder #-}
-- | Reports the remainder when number1 is divided by number2. 
remainder :: Int -> Int -> Int
remainder = rem

{-# WARNING variance "TODO" #-}
-- | Reports the sample variance of a list of numbers. Ignores other types of items. 
variance :: t
variance = todo


{-# WARNING standard_deviation "TODO" #-}
-- | Reports the sample standard deviation of a list of numbers. Ignores other types of items. 
standard_deviation :: t -> t1
standard_deviation _l = todo

{-# WARNING subtract_headings "TODO? maybe it is finished" #-}
-- | Computes the difference between the given headings, that is, the number of degrees in the smallest angle by which heading2 could be rotated to produce heading1. 
subtract_headings :: STMorIO m => Double -> Double -> C _s _s' m Double
subtract_headings h1 h2 = let 
    h1' = if h1 < 0 || h1 >= 360
          then (h1 `mod_` 360 + 360) `mod_` 360
          else h1
    h2' = if h2 < 0 || h2 >= 360
          then (h2 `mod_` 360 + 360) `mod_` 360
          else h2
    diff = h1' - h2'
                           in return $
                             if diff > -180 && diff <= 180
                             then diff
                             else if diff > 0
                                  then diff - 360
                                  else diff + 360

-- let r1 = h2 - h1 `mod_` 180
                          --     r2 = h1 - h2 `mod_` 180
                          -- in return $
                          --   if abs r1 < abs r2
                          --   then if h2 > 180 then -r1 else r1
                          --   else if h2 > 180 then -r2 else r2

-- | The link makes itself invisible. 
hide_link :: C Link _s' STM ()
hide_link = do
  (MkLink {lhiddenp_ = h},_) <- Reader.ask
  lift $ writeTVar h True

-- | The turtle becomes visible again. 
show_link :: C Link _s' STM ()
show_link = do
  (MkLink {lhiddenp_ = h},_) <- Reader.ask
  lift $ writeTVar h False


-- | Reports the distance between the endpoints of the link. 
link_length :: C Link _s' STM Double
link_length = do
    (MkLink {end1_ =f, end2_ = t},_) <- Reader.ask
    [MkTurtle {xcor_ = fx, ycor_ = fy}] <- turtle f
    [MkTurtle {xcor_ = tx, ycor_ = ty}] <- turtle t
    x <- lift $ readTVar fx
    y <- lift $ readTVar fy
    x' <- lift $ readTVar tx
    y' <- lift $ readTVar ty
    return $ sqrt (delta x x' (max_pxcor_ conf) ^ 2 + 
                delta y y' (max_pycor_ conf) ^ 2)



-- | Report the undirected link between turtle and the caller. If no link exists then it reports nobody. 
-- link_with :: [Turtle] -> C Turtle _s' STM [Link]
-- link_with [MkTurtle {who_=x}] = do
--    (MkTurtle {who_=y},_) <- Reader.ask
--    lxy <- link x y
--    lyx <- link y x
--    return $ case (lxy,lyx) of
--               ([Nobody], [Nobody]) -> [Nobody]
--               ([Nobody], _) -> error "directed link"
--               ([LinkRef _ _], [LinkRef _ _]) -> lxy -- return arbitrary 1 of the two link positions
--               (_, [Nobody]) -> error "directed link"
--               _ -> throw DevException
-- link_with a = throw $ TypeException "single turtle"

  
-- | Report the directed link from turtle to the caller. If no link exists then it reports nobody. 
-- in_link_from :: [Turtle] -> C Turtle _s' STM [Link]
-- in_link_from [MkTurtle {who_=x}] = do
--    (MkTurtle {who_=y},_) <- Reader.ask
--    lxy <- link x y
--    lyx <- link y x
--    return $ case (lxy,lyx) of
--               ([Nobody], _) -> [Nobody]
--               (_, [Nobody]) -> lxy
--               ([LinkRef _ _], [LinkRef _ _]) -> error "undirected link"
--               _ -> throw DevException
-- in_link_from a = throw $ TypeException "turtle"



-- | Reports the directed link from the caller to turtle. If no link exists then it reports nobody. 
-- out_link_to :: [Turtle] -> C Turtle _s' STM [Link]
-- out_link_to [MkTurtle {who_=x}] = do
--    (MkTurtle {who_=y},_) <- Reader.ask
--    lxy <- link x y
--    lyx <- link y x
--    return $ case (lyx,lxy) of
--               ([Nobody], _) -> [Nobody]
--               (_, [Nobody]) -> lyx
--               ([LinkRef _ _], [LinkRef _ _]) -> error "undirected link"
--               _ -> throw DevException
-- out_link_to a = throw $ TypeException "turtle" (head a)


{-# WARNING my_links "TODO" #-}
-- | Reports an agentset of all undirected links connected to the caller. 
my_links :: C Turtle _s' STM [Link]
my_links = do
  (MkTurtle {who_=x},_) <- Reader.ask 
  ls <- lift $ readTVar __links
  todo
  -- return $ map (uncurry LinkRef) $ M.assocs $ M.intersection (M.filterWithKey (\ (f,_) _ -> f == x) ls) (M.filterWithKey (\ (_,t) _ -> t == x) ls)

{-# WARNING my_out_links "TODO" #-}
-- | Reports an agentset of all the directed links going out from the caller to other nodes. 
my_out_links :: C Turtle _s' STM [Link]
my_out_links = do
  (MkTurtle {who_=x},_) <- Reader.ask 
  ls <- lift $ readTVar __links
  todo 
  -- return $ map (uncurry LinkRef) $ M.assocs $ M.filterWithKey (\ (f,_) _ -> f == x) ls

{-# WARNING my_in_links "TODO" #-}
-- |  Reports an agentset of all the directed links coming in from other nodes to the caller. 
my_in_links :: C Turtle _s' STM [Link]
my_in_links = do
  (MkTurtle {who_=x},_) <- Reader.ask 
  ls <- lift $ readTVar __links
  todo
  -- return $ map (uncurry LinkRef) $ M.assocs $ M.filterWithKey (\ (_,t) _ -> t == x) ls

-- | Ties end1 and end2 of the link together. If the link is a directed link end1 is the root turtle and end2 is the leaf turtle. The movement of the root turtle affects the location and heading of the leaf turtle. If the link is undirected the tie is reciprocal so both turtles can be considered root turtles and leaf turtles. Movement or change in heading of either turtle affects the location and heading of the other turtle. 
tie :: C Link _s' STM ()
tie = do
  (MkLink {tie_mode = t},_) <- Reader.ask
  lift $ writeTVar t Fixed

-- | Unties end2 from end1 (sets tie-mode to "none") if they were previously tied together. If the link is an undirected link, then it will untie end1 from end2 as well. It does not remove the link between the two turtles. 
untie :: C Link _s' STM ()
untie = do
  (MkLink {tie_mode = t},_) <- Reader.ask
  lift $ writeTVar t None

end1 :: C Link _s' STM [Turtle]
end1 = do
  (MkLink {end1_ = e1},_) <- Reader.ask
  turtle e1

end2 :: C Link _s' STM [Turtle]
end2 = do
  (MkLink {end2_ = e2},_) <- Reader.ask
  turtle e2


-- | lifting STM to IO, a wrapper to 'atomically' that optionally (based on a CPP flag) can capture STM statistics 
atomic :: C _s _s' STM a -> C _s _s' IO a
atomic comms = 
#ifndef STATS_STM
       Reader.mapReaderT atomically comms
#else
       do
         (_, s, _, _) <- Reader.ask 
         lift $ increaseSuccessfulSTM s
         Reader.mapReaderT atomically (lift (unsafeIOToSTM $ increaseTotalSTM s) >> comms)
    where
        increaseSuccessfulSTM s = case s of
                                     TurtleRef _ t -> atomicModifyIORef'  (tsuccstm t) (\ x -> (x+1, ()))
                                     PatchRef _ p -> atomicModifyIORef'  (psuccstm p) (\ x -> (x+1, ()))
                                     LinkRef _ l -> atomicModifyIORef'  (lsuccstm l) (\ x -> (x+1, ()))
                                     ObserverRef _ -> return ()
                                     Nobody -> throw DevException

{-# NOINLINE increaseTotalSTM #-}
-- | Internal
increaseTotalSTM :: AgentRef -> IO ()
increaseTotalSTM s = case s of
                       TurtleRef _ t -> atomicModifyIORef'  (ttotalstm t) (\ x -> (x+1, ()))
                       PatchRef _ p -> atomicModifyIORef'  (ptotalstm p) (\ x -> (x+1, ()))
                       LinkRef _ l -> atomicModifyIORef'  (ltotalstm l) (\ x -> (x+1, ()))
                       ObserverRef _ -> return ()
                       Nobody -> throw DevException

#endif

instance Agent Turtle where
    ask f as = do
      (s,_) <- Reader.ask
      lift $ do
        mapM_ (\ (core, asSection) -> 
                   ThreadG.forkOn core __tg $ sequence_ [Reader.runReaderT f (a,s) | a <- asSection]
              ) (split numCapabilities as)
        ThreadG.wait __tg

    of_ f as = do
      (s,_) <- Reader.ask
      lift $ do
             ws <- mapM (\ (core, asi) -> liftM snd $ Thread.forkOn core (sequence [Reader.runReaderT f (a,s) | a <- asi])) (split numCapabilities as)
             rs <- sequence [Thread.result =<< w | w <- ws]
             return $ concat rs -- lists traversals can be optimized

instance Agent Patch where
    ask f as = do
      (s,_) <- Reader.ask
      lift $ do
        mapM_ (\ (core, asSection) -> 
                   ThreadG.forkOn core __tg $ sequence_ [Reader.runReaderT f (a,s) | a <- asSection]
              ) (split numCapabilities as)
        ThreadG.wait __tg
      
    of_ f as = do
      (s,_) <- Reader.ask
      lift $ do
             ws <- mapM (\ (core, asi) -> liftM snd $ Thread.forkOn core (sequence [Reader.runReaderT f (a,s) | a <- asi])) (split numCapabilities as)
             rs <- sequence [Thread.result =<< w | w <- ws]
             return $ concat rs -- lists traversals can be optimized

instance Agent Link where
    ask f as = do
      (s,_) <- Reader.ask
      lift $ do
        mapM_ (\ (core, asSection) -> 
                   ThreadG.forkOn core __tg $ sequence_ [Reader.runReaderT f (a,s) | a <- asSection]
              ) (split numCapabilities as)
        ThreadG.wait __tg
      
    of_ f as = do
      (s,_) <- Reader.ask
      lift $ do
             ws <- mapM (\ (core, asi) -> liftM snd $ Thread.forkOn core (sequence [Reader.runReaderT f (a,s) | a <- asi])) (split numCapabilities as)
             rs <- sequence [Thread.result =<< w | w <- ws]
             return $ concat rs -- lists traversals can be optimized





        -- do
        -- ws <- case split_ conf of
        -- "none" -> 
        -- "horizontal" -> lift $ mapM (\ (processor_i, asi) -> 
        --                            liftM snd $ if processor_i == numCapabilities -- not belonging to our scheduler
        --                                        then ThreadG.forkIO __tg (sequence_ [Reader.runReaderT f (tw, a, p, s) | a <- asi])
        --                                        else if null asi
        --                                             then return undefined -- optimization, so not to spawn a thread if there is nothing to execute
        --                                             else ThreadG.forkOn processor_i (sequence_ [Reader.runReaderT f (tw, a, p, s) | a <- asi])) (hsplit numCapabilities as)
        -- "vertical" -> lift $ mapM (\ (processor_i, asi) -> 
        --                            liftM snd $ if processor_i == numCapabilities -- not belonging to our scheduler
        --                                        then ThreadG.forkIO __tg (sequence_ [Reader.runReaderT f (tw, a, p, s) | a <- asi])
        --                                        else if null asi
        --                                             then return undefined -- optimization, so not to spawn a thread if there is nothing to execute
        --                                             else ThreadG.forkOn processor_i (sequence_ [Reader.runReaderT f (tw, a, p, s) | a <- asi])) (vsplit numCapabilities as)
        -- "both" -> error "the both splitting is not ready yet. TODO"
        -- _ -> error "not valid splitting option"
                      
 -- lift $ sequence_ ws 

askTurtles :: C Turtle _s IO a -> C _s _s' IO ()
askTurtles f = do
      (s,_) <- Reader.ask
      lift $ do
        ts <- readTVarIO __turtles
        mapM_ (\ (tslice,core) ->
                  ThreadG.forkOn core __tg $ mapM_ (\ t -> Reader.runReaderT f (t,s)) tslice
             ) (zip (concatMap IM.splitRoot (IM.splitRoot ts)) [1..4])
        ThreadG.wait __tg



{-# WARNING askPatches "TODO: both splitting" #-}
-- | The specified agent or agentset runs the given commands. 
askPatches :: C Patch _s IO a -> C _s _s' IO ()
askPatches f = do
    (s,_) <- Reader.ask
    lift $ do
      -- mapM_ (\ (core, ycorSlice) -> 
      --        -- this splits it in rows
      --        ThreadG.forkOn core __tg $ sequence_ [Reader.runReaderT f (__patches ! (x,y), s) | x <- [min_pxcor_ conf..max_pxcor_ conf], y <- ycorSlice]
      --       ) (split numCapabilities [min_pycor_ conf .. max_pycor_ conf])
      -- mapM_ (\ (core, ycorSlice) -> 
      --            ThreadG.forkOn core __tg $ sequence_ [Reader.runReaderT f (__patches ! (x,y),s) | x <- ycorSlice, y <- [min_pycor_ conf..max_pycor_ conf]]
      --       ) (split numCapabilities [min_pxcor_ conf .. max_pxcor_ conf])
      -- let (v1, v2) = V.splitAt (V.length __patches `div` 2) __patches
      -- mapM (\ (vslice,core) -> 
      --           ThreadG.forkOn core __tg $ V.mapM_ (\ row -> V.mapM_ (\ p -> Reader.runReaderT f (p,s)) row) vslice
      --       )
      --       (zip [v1,v2] [1,2])
      mapM (\ (start,size,core) -> 
                 ThreadG.forkOn core __tg $ V.mapM_ (\ row -> V.mapM_ (\ p -> Reader.runReaderT f (p,s)) row) (V.unsafeSlice start size __patches)
             )
             (splitN (max_pxcor_ conf - min_pxcor_ conf + 1) numCapabilities)

      ThreadG.wait __tg                               
          where
            splitN :: Int -> Int -> [(Int,Int,Int)]
            splitN width n = let (q,r) = width `quotRem` n
                                 splitN' (0,s,l) c = (0, q+s, (s,q,c):l)
                                 splitN' (r',s,l) c = (r'-1, q+s+1, (s,q+1,c):l)
                             in  case (foldl' splitN' (r,0,[]) [1..n]) of
                                   (_,_,res) -> res


    -- where
    --   splitN :: Int -> Int -> [(Int,Int,Int)]
    --   splitN width n = let (q,r) = width `quotRem` n
    --                  splitN' (0,s,l) c = (0, q+s, (s,q,c):l)
    --                  splitN' (r',s,l) c = (r'-1, q+s+1, (s,q+1,c):l)
    --              in  case (foldl' splitN' (r,0,[]) [1..n]) of
    --                    (_,_,res) -> res
                                -- this splits it in rows
                                --      ThreadG.forkOn core __tg $ sequence_ [Reader.runReaderT f (PatchRef (x,y) (__patches ! (x,y)), p, s) | x <- [min_pxcor_ conf..max_pxcor_ conf], y <- ycorSlice]
                                -- ) (split numCapabilities [min_pycor_ conf .. max_pycor_ conf])
                                -- this splits it in columns, for some strange reason this works slightly faster. Maybe because data locality works better with smaller chunks than having largely-contiguous chunks

-- splitN :: Int -> Int -> [(Int,Int,Int)]
-- splitN width n = let (q,r) = width `quotRem` n
--                      splitN' (0,s,l) c = (0, q+s, (s,q,c):l)
--                      splitN' (r',s,l) c = (r'-1, q+s+1, (s,q+1,c):l)
--                  in  case (foldl' splitN' (r,0,[]) [1..n]) of
--                        (_,_,res) -> res


-- | Internal
split :: Int -> [a] -> [(Int, [a])]
split 1 l = [(1,l)]
split n l = let (d,m) = length l `quotRem` n
                split' 0 _ _ = []
                split' x 0 l' = let (t, rem_list) = splitAt d l'
                                in (x,t) : split' (x-1) 0 rem_list
                split' x m' l' = let (t, rem_list) = splitAt (d+1) l'
                                 in (x,t) : split' (x-1) (m'-1) rem_list
            in split' n m l

-- | Internal
-- vsplit :: Int -> [AgentRef] -> [(Int, [AgentRef])]
-- vsplit n as = IM.toAscList $ foldl' (\ im a -> case a of
--                                             PatchRef (x1, _) _ -> IM.insertWith (++) ((x1 + max_x) `div` sector) [a] im
--                                             TurtleRef _ (MkTurtle {init_xcor_ = ix}) -> IM.insertWith (++) ((ix + max_x) `div` sector) [a] im
--                                             _ -> IM.insertWith (++) n [a] im -- it is not a patch or a turtle, so it is a link, it should be scheduled by GHC and not by us, put it at the extra n
--                                 ) IM.empty as
--               where max_x = max_pxcor_ conf
--                     min_x = min_pxcor_ conf
--                     sector = (max_x - min_x) `div` n

-- -- | Internal
-- hsplit :: Int -> [AgentRef] -> [(Int, [AgentRef])]
-- hsplit n as = IM.toAscList $ foldl' (\ im a -> case a of
--                                             PatchRef (_, y1) _ -> IM.insertWith (++) ((y1 + max_y) `div` sector) [a] im
--                                             TurtleRef _ (MkTurtle {init_ycor_ = iy}) -> IM.insertWith (++) ((iy + max_y) `div` sector) [a] im
--                                             _ -> IM.insertWith (++) n [a] im -- it is not a patch, so it should be scheduled by GHC and not by us, put it at the extra n
--                                 ) IM.empty as
--               where max_y = max_pycor_ conf
--                     min_y = min_pycor_ conf
--                     sector = (max_y - min_y) `div` n


-- | For an agent, reports the value of the reporter for that agent (turtle or patch). 
--  For an agentset, reports a list that contains the value of the reporter for each agent in the agentset (in random order). 
                 
  
-- | Takes two inputs: an agentset and a boolean reporter. Reports a new agentset containing only those agents that reported true 
-- in other words, the agents satisfying the given condition. 
with :: Agent a => C a _s IO Bool -> [a] -> C _s _s' IO [a]
with f as = do
  res <- f `of_` as
  return $ foldr (\ (a, r) l -> if r then a:l else l) [] (zip as res)

{-# SPECIALIZE  with :: C Turtle p IO Bool -> [Turtle] -> C p p' IO [Turtle] #-}
{-# SPECIALIZE  with :: C Patch p IO Bool -> [Patch] -> C p p' IO [Patch] #-}
{-# SPECIALIZE  with :: C Link p IO Bool -> [Link] -> C p p' IO [Link] #-}

{-# WARNING loop  "TODO: use MaybeT or ErrorT" #-}
-- |  Runs the list of commands forever, or until the current procedure exits through use of the stop command or the report command. 
-- NB: Report command will not stop it in HLogo, only the stop primitive. 
-- This command is only run in IO, bcs the command has been implemented
-- using exceptions and exceptions don't work the same in STM. Also
-- it avoids common over-logging that can happen in STM.
loop :: C _s _s' IO a -> C _s _s' IO ()
loop c = forever c `catchIO` \ StopException -> return ()

{-# INLINE stop #-}
-- | This agent exits immediately from the enclosing to-procedure that was called from 'ask', or ask-like construct (e.g. crt, hatch, sprout). Only the current procedure stops, not all execution for the agent. Also can exit from a top-level (observer) procedure.
stop :: a
stop = throw StopException

{-# WARNING while  "TODO: use MaybeT or ErrorT" #-}
-- | If reporter reports false, exit the loop. Otherwise run commands and repeat. 
-- This command is only run in IO, bcs the command has been implemented
-- using exceptions and exceptions don't work the same in STM. Also
-- it avoids common over-logging that can happen in STM.
while :: C _s _s' IO Bool -> C _s _s' IO a -> C _s _s' IO ()
while r c = r >>= \ res -> when res $ (c >> while r c) `catchIO` (\ StopException -> return ())

-- Type-safe Casts

-- is_turtlep :: (Monad m, Typeable a) => a -> C m Bool
-- is_turtlep a = return $ maybe False (\case [TurtleRef _ _] -> True
--                                            _ -> False
--                                     )
--                                          (cast a :: Maybe [AgentRef])

-- is_patchp :: (Monad m, Typeable a) => a -> C m Bool
-- is_patchp a = return $ maybe False (\case [PatchRef _ _] -> True
--                                           _ -> False)
--                                          (cast a :: Maybe [AgentRef])

-- is_agentp :: (Monad m, Typeable a) => a -> C m Bool
-- is_agentp a = return $ maybe False (\case [Nobody] -> False                                       
--                                           [_] -> True -- check for a single agent
--                                           _ -> False
--                                    ) (cast a :: Maybe [AgentRef])

-- -- alternative but slower implementation is_agentp a = is_turtlep a || is_patchp a


-- -- | Reports true if value is of the given type, false otherwise. 
-- is_patch_setp :: (Monad m, Typeable a) => [a] -> C m Bool
-- is_patch_setp ps = do
--   res <- mapM is_patchp ps
--   return $ and res


-- -- | Reports true if value is of the given type, false otherwise. 
-- is_turtle_setp :: (Monad m, Typeable a) => [a] -> C m Bool
-- is_turtle_setp ps = do
--   res <- mapM is_turtlep ps
--   return $ and res

-- -- | Reports true if value is of the given type, false otherwise.  
-- is_agentsetp :: (Monad m, Typeable a) => [a] -> C m Bool
-- is_agentsetp ps = do 
--   res <- mapM (\ a -> do
--                 ip <- is_patchp a
--                 it <- is_turtlep a 
--                 il <- is_linkp a
--                 return $ ip || it || il) ps
--   return $ and res

-- Not used, because EDSL, using internal lambda abstractions
-- is_command_taskp
-- is_reporter_taskp

{-# WARNING is_listp "TODO" #-}
--is_listp :: (Monad m, Typeable a) => a -> C m Bool
--is_listp :: (Typeable a, Typeable t) => t -> [a]
--is_listp l =  (cast l :: Typeable a => Maybe [a])
is_listp :: t
is_listp = todo

is_stringp :: (STMorIO m, Typeable a) => a -> C _s _s' m Bool
is_stringp s = return $ isJust (cast s :: Maybe String)

is_numberp :: (STMorIO m, Typeable a) => a -> C _s _s' m Bool
is_numberp n = return $ is_intp || is_doublep 
               where
                 is_intp = isJust (cast n :: Maybe Int)
                 is_doublep = isJust (cast n :: Maybe Double)

-- is_linkp :: (Monad m, Typeable a) => a -> C m Bool
-- is_linkp l = return $ maybe False (\case [LinkRef _ _] -> True
--                                          _ -> False)
--                                          (cast l :: Maybe [AgentRef])

-- is_directed_linkp :: (Monad m, Typeable a) => a -> C m Bool
-- is_directed_linkp l = return $ maybe False (\case [LinkRef _ (MkLink {directed_ = d})] -> d
--                                                   _ -> False)
--                                          (cast l :: Maybe [AgentRef])

-- is_undirected_linkp :: (Monad m, Typeable a) => a -> C m Bool
-- is_undirected_linkp = liftM not . is_directed_linkp



-- {-# WARNING is_link_setp "TODO: would require a datatype distinction between agentrefs" #-}
-- | Checks only the 1st element
-- is_link_setp :: (Monad m, Typeable a) => [a] -> C m Bool
-- is_link_setp (l:_) = is_linkp l
-- is_link_setp _ = throw DevException

-- | This turtle creates number new turtles. Each new turtle inherits of all its variables, including its location, from its parent. (Exceptions: each new turtle will have a new who number)
hatch :: Int -> C Turtle _s' STM [Turtle]
hatch n = do
#ifdef STATS_STM
    (MkTurtle _w bd c h x y s l lc hp sz ps pm tarr _ _ix _iy _tt _ts, _) <- Reader.ask
#else
    (MkTurtle _w bd c h x y s l lc hp sz ps pm tarr _ _ix _iy, _) <- Reader.ask
#endif
    let b = bounds tarr
    -- todo: this whole code could be made faster by readTVar of the attributes only once and then newTVar multiple times from the 1 read
    let newArray = return . listArray b =<< sequence [newTVar =<< readTVar (tarr ! i) | i <- [fst b.. snd b]]
    let newTurtles w = return . IM.fromAscList =<< sequence [do
                                                                        t <- MkTurtle <$>
                                                                            return i <*>
                                                                            (newTVar =<< readTVar bd) <*>
                                                                            (newTVar =<< readTVar c) <*>
                                                                            (newTVar =<< readTVar h) <*>
                                                                            (newTVar =<< readTVar x) <*>
                                                                            (newTVar =<< readTVar y) <*>
                                                                            (newTVar =<< readTVar s) <*> 
                                                                            (newTVar =<< readTVar l) <*>
                                                                            (newTVar =<< readTVar lc) <*>
                                                                            (newTVar =<< readTVar hp) <*>
                                                                            (newTVar =<< readTVar sz) <*>
                                                                            (newTVar =<< readTVar ps) <*>
                                                                            (newTVar =<< readTVar pm) <*>
                                                                            newArray <*>
                                                                            newTVar (mkStdGen i) <*> 
                                                                            liftA round (readTVar x) <*> 
                                                                            liftA round (readTVar y)
#ifdef STATS_STM                                                                            
                                                                            <*> pure (unsafePerformIO (newIORef 0)) <*> 
                                                                            pure (unsafePerformIO (newIORef 0))
#endif
                                                                        return (i, t) | i <- [w..w+n-1]]
    oldWho <- lift $ readTVar __who
    lift $ modifyTVar' __who (n +)
    ns <- lift $ newTurtles oldWho
    lift $ modifyTVar' __turtles (`IM.union` ns) 
    return $ IM.elems ns -- todo: can be optimized

-- | The turtle sets its x and y coordinates to be the same as the given agent's.
-- (If that agent is a patch, the effect is to move the turtle to the center of that patch.) 
-- move_to :: TurtlePatch a => [a] -> C Turtle _s' STM ()
-- move_to [a] = do
--   (MkTurtle {xcor_ = tx, ycor_ = ty},_) <- Reader.ask
--   (MkPatch {pxcor_ = px, pycor_ = py}) <- patch_on_ a
--   lift $ writeTVar tx (fromIntegral px) >> writeTVar ty (fromIntegral py)
-- move_to _ = throw $ TypeException "single turtle or patch"

{-# INLINE turtles_on #-}
-- | Reports an agentset containing all the turtles that are on the given patch or patches, or standing on the same patch as the given turtle or turtles. 
turtles_on :: (Player s, TurtlePatch a) => [a] -> C s _s' IO [Turtle]
turtles_on as = liftM concat $ turtles_here `of_` as
    
{-# WARNING at_points "TODO: also has to support the Observer as Caller. A TurtleObserver typeclass" #-}
-- |  Reports a subset of the given agentset that includes only the agents on the patches the given distances away from this agent. The distances are specified as a list of two-item lists, where the two items are the x and y offsets.
-- If the caller is the observer, then the points are measured relative to the origin, in other words, the points are taken as absolute patch coordinates.
-- If the caller is a turtle, the points are measured relative to the turtle's exact location, and not from the center of the patch under the turtle. 
at_points :: (TurtlePatch a) => [a] -> [(Double, Double)] -> C Turtle _s' STM [a]
at_points [] _ = return []
at_points (_a:_as) _ds = todo
  -- do
  -- (_,_,s,_,_) <- Reader.ask
  -- (x,y) <- case s of
  --           ObserverRef _ -> return (0,0)
  --           PatchRef (px, py) _ -> return (fromIntegral px, fromIntegral py)
  --           TurtleRef _ (MkTurtle {xcor_ = tx, ycor_ = ty}) -> lift $ liftM2 (,) (readTVar tx) (readTVar ty)
  --           LinkRef _ _ -> throw $ TypeException "observer/patch/turtle" s
  --           Nobody -> throw DevException

-- | Runs the given commands only if it's been more than number seconds since the last time this agent ran them in this context. Otherwise, the commands are skipped. 
-- | NB: Works differently than NetLogo, in that only the calling thread is suspended, not the whole simulation
every :: Double -> C _s _s' IO a -> C _s _s' IO ()
every n a = a >> wait n

{-# INLINE wait #-}
-- | Wait the given number of seconds. (This needn't be an integer; you can specify fractions of seconds.) Note that you can't expect complete precision; the agent will never wait less than the given amount, but might wait slightly more. 
-- | NB: Works differently than NetLogo, in that only the calling thread is suspended, not the whole simulation
wait :: Double -> C _s _s' IO ()
wait n = lift $ threadDelay (round $ n * 1000000)

{-# DEPRECATED unsafe_show "it is slightly faster than show since it does not involve any STM, but it should not matter that much and also printing is discourage on real benchmarking." #-} 
-- | Considered unsafe; the output may be mangled, because of many threads writing to the same output
unsafe_show :: (Player s, Show a) => a -> C s _s' IO ()
unsafe_show a = do
  (r,_) <- Reader.ask
  lift $ putStrLn $ Prelude.show r ++ ": " ++ Prelude.show a

{-# DEPRECATED unsafe_print "it is slightly faster than print since it does not involve any STM, but it should not matter that much and also printing is discourage on real benchmarking." #-} 
-- | Considered unsafe; the output may be mangled, because of many threads writing to the same output
unsafe_print :: Show a => a -> C _s _s' IO ()
unsafe_print a = lift $ Prelude.print a

stats_stm :: C Observer () IO Double
stats_stm = 
#ifndef STATS_STM 
   error "library not compiled with stats-stm flag enabled"
#else
   do
    (tw, _, _, _) <- Reader.ask
    MkWorld wts wls <- lift $ readTVarIO tw
    (vtt, vts) <- lift $ F.foldlM (\ (acct, accs) (MkTurtle {ttotalstm=tt, tsuccstm=ts}) -> do
                                  rtt <- readIORef tt
                                  rts <- readIORef ts
                                  return (acct+rtt, accs+rts)) (0,0) wts
    (vpt, vps) <- lift $ F.foldlM (\ (acct, accs) (MkPatch {ptotalstm=pt, psuccstm=ps}) -> do
                                  rpt <- readIORef pt
                                  rps <- readIORef ps
                                  return (acct+rpt, accs+rps)) (0,0) __patches
    (vlt, vls) <- lift $ F.foldlM (\ (acct, accs) (MkLink {ltotalstm=pt, lsuccstm=ps}) -> do
                                  rlt <- readIORef pt
                                  rls <- readIORef ps
                                  return (acct+rlt, accs+rls)) (0,0) wls
    let total = fromIntegral $ vtt+vpt+vlt
    let suc = fromIntegral $ vts+vps+vls
    return $ (total - suc) / total
#endif


with_breed :: TurtleLink s => (String -> String) -> C s _s' STM ()
with_breed f = do
  (s,_) <- Reader.ask
  lift $ modifyTVar' (breed_ s) f

with_color :: TurtleLink s => (Double -> Double) -> C s _s' STM ()
with_color f = do
  (s,_) <- Reader.ask
  lift $ modifyTVar' (color_ s) f

with_heading :: (Double -> Double) -> C Turtle _s' STM ()
with_heading f = do
  (MkTurtle {heading_ = tb},_) <- Reader.ask
  lift $ modifyTVar' tb f

with_shape :: TurtleLink s => (String -> String) -> C s _s' STM ()
with_shape f = do
  (s,_) <- Reader.ask
  lift $ modifyTVar' (shape_ s) f

with_label :: TurtleLink s => (String -> String) -> C s _s' STM ()
with_label f = do
  (s,_) <- Reader.ask
  lift $ modifyTVar' (label_ s) f

with_label_color :: TurtleLink s => (Double -> Double) -> C s _s' STM ()
with_label_color f = do
  (s,_) <- Reader.ask
  lift $ modifyTVar' (label_color_ s) f

with_size :: (Double -> Double) -> C Turtle _s' STM ()
with_size f = do
  (MkTurtle {size_ = tb},_) <- Reader.ask
  lift $ modifyTVar' tb f

with_pcolor :: TurtlePatch s => (Double -> Double) -> C s _s' STM ()
with_pcolor f = do
  (s,_) <- Reader.ask
  (MkPatch {pcolor_ = tb}) <- patch_on_ s
  lift $ modifyTVar' tb f

with_plabel :: TurtlePatch s => (String -> String) -> C s _s' STM ()
with_plabel f = do
  (s,_) <- Reader.ask
  (MkPatch {plabel_ = tb}) <- patch_on_ s
  lift $ modifyTVar' tb f

with_plabel_color :: TurtlePatch s => (Double -> Double) -> C s _s' STM ()
with_plabel_color f = do
  (s,_) <- Reader.ask
  (MkPatch {plabel_color_ = tb}) <- patch_on_ s
  lift $ modifyTVar' tb f

snapshot :: C Observer () IO ()
snapshot = do
             ticksNow <- ticks
             ps <- patch_size
             max_x <- max_pxcor
             min_x <- min_pycor
             let sizeSpec = Diag.mkWidth (fromIntegral (ps * (max_x + abs min_x + 1)))
             let output = "snapshot" ++ Prelude.show (round ticksNow :: Int) ++ ".eps"
             prs <- patches
             diagPatches <- lift $ mapM (\ (MkPatch {pxcor_ = px, pycor_ = py, pcolor_ = pc, plabel_ = pl}) -> do 
                                   c <- readTVarIO pc
                                   t <- readTVarIO pl
                                   let [r,g,b] = extract_rgb c
                                   return (Diag.p2 (fromIntegral px, fromIntegral py), Diag.text t Diag.# Diag.fc (sRGB24 255 255 255) Diag.<> Diag.square 1 Diag.# Diag.fc (sRGB24 r g b) :: Diag.Diagram Postscript)
                                ) prs 
             trs <- turtles
             diagTurtles <- lift $ mapM (\ (MkTurtle {xcor_ = tx, ycor_ = ty, tcolor_ = tc, heading_ = th, size_ = ts}) -> do 
                                          x <- readTVarIO tx
                                          y <- readTVarIO ty
                                          c <- readTVarIO tc
                                          h <- readTVarIO th
                                          s <- readTVarIO ts
                                          let [r,g,b] = extract_rgb c
                                          return (Diag.p2 (x, y), Diag.eqTriangle s Diag.# Diag.fc (sRGB24 r g b) Diag.# Diag.scaleX 0.5 Diag.# Diag.rotate (-h Diag.@@ Diag.deg) :: Diag.Diagram Postscript)
                                ) trs

             
             lift $ Diag.renderDia Postscript (PostscriptOptions output sizeSpec EPS) (Diag.position diagTurtles `Diag.atop` Diag.position diagPatches)

extract_rgb :: Double -> [Word8]
extract_rgb c | c == 0 = [0,0,0]
              | c == 9.9 = [255,255,255]
              | otherwise = let colorTimesTen = truncate(c * 10)
                                baseIndex = colorTimesTen `div` 100
                                colorsRGB = [
                                 140, 140, 140, -- gray (5)
                                 215, 48, 39, -- red (15)
                                 241, 105, 19, -- orange (25)
                                 156, 109, 70, -- brown (35)
                                 237, 237, 47, -- yellow (45)
                                 87, 176, 58, -- green (55)
                                 42, 209, 57, -- lime (65)
                                 27, 158, 119, -- turquoise (75)
                                 82, 196, 196, -- cyan (85)
                                 43, 140, 190, -- sky (95)
                                 50, 92, 168, -- blue (105)
                                 123, 78, 163, -- violet (115)
                                 166, 25, 105, -- magenta (125)
                                 224, 126, 149, -- pink (135)
                                 0, 0, 0, -- black
                                 255, 255, 255 -- white
                                          ]
                                r = colorsRGB !! (baseIndex * 3 + 0)
                                g = colorsRGB !! (baseIndex * 3 + 1)
                                b = colorsRGB !! (baseIndex * 3 + 2)
                                step = fromIntegral (colorTimesTen `rem` 100 - 50) / 50.48 + 0.012 :: Double
                            in
                              if step < 0
                              then [truncate(fromIntegral r * step) +r, truncate(fromIntegral g*step)+g, truncate(fromIntegral b*step)+b]
                              else [truncate((255 - fromIntegral r)*step)+r, truncate((255 - fromIntegral g)*step)+g, truncate((255 - fromIntegral b)*step)+b]

{-# WARNING approximate_rgb "TODO" #-}
approximate_rgb :: a
approximate_rgb = todo

-- | Reports an agentset containing the 8 surrounding patches
neighbors :: (STMorIO m, TurtlePatch s) => C s _s' m [Patch]
neighbors = do
    (s,_) <- Reader.ask
    (MkPatch {pxcor_ = x, pycor_ = y}) <- patch_on_ s
    patch_set [p (x-1) (y-1),
               p (x-1) y,
               p (x-1) (y+1),
               p x (y-1),
               p x (y+1),
               p (x+1) (y-1),
               p (x+1) y,
               p (x+1) (y+1)
              ]
    where p x y = if (not (horizontal_wrap_ conf) && (x > (max_pxcor_ conf) || x < (min_pxcor_ conf))) || (not (vertical_wrap_ conf) && (y >  (max_pycor_ conf) || y <  (min_pycor_ conf)))
                  then return []
                  else patch (fromIntegral x) (fromIntegral y)

-- | Reports an agentset containing the 4 surrounding patches
neighbors4 :: (STMorIO m, TurtlePatch s) => C s _s' m [Patch]
neighbors4 = do
    (s,_) <- Reader.ask
    (MkPatch {pxcor_ = x, pycor_ = y}) <- patch_on_ s
    patch_set [p (x-1) y,
               p (x+1) y,
               p x (y-1),
               p x (y+1)
              ]
    where p x y = if (not (horizontal_wrap_ conf) && (x > (max_pxcor_ conf) || x < (min_pxcor_ conf))) || (not (vertical_wrap_ conf) && (y >  (max_pycor_ conf) || y < (min_pycor_ conf)))
                  then return []
                  else patch (fromIntegral x) (fromIntegral y)



-- | A class to take advantage of faster 'readTVarIO'. Any commands that do not do STM side-effects (IO effects allowed)
--  or only depend on 'readTVar', belong here. The correct lifting (STM or IO) is left to type inference.
-- class Monad m => STMorIO m where

-- |  Reports an agentset containing all the turtles on the caller's patch (including the caller itself if it's a turtle). 
turtles_here :: (TurtlePatch s, STMorIO m) => C s _s' m [Turtle]
turtles_here = do
    (s,_) <- Reader.ask
    (MkPatch {pxcor_ = px, pycor_ = py}) <- patch_on_ s
    ts <- turtles
    lift $ filterM (\ (MkTurtle {xcor_ = x, ycor_ = y}) -> do 
                      x' <- readTVarSI x
                      y' <- readTVarSI y
                      return $ round x' == px && round y' == py
                   ) ts

-- |  Reports an agentset containing the turtles on the patch (dx, dy) from the caller. (The result may include the caller itself if the caller is a turtle.) 
turtles_at :: (TurtlePatch s, STMorIO m) => Double -> Double -> C s _s' m [Turtle] -- ^ dx -> dy -> CSTM (Set AgentRef)
turtles_at x y = do
    [MkPatch {pxcor_=px, pycor_=py}] <- patch_at x y
    ts <- turtles
    lift $ filterM (\ (MkTurtle {xcor_ = tx, ycor_ = ty}) -> do 
                      x' <- readTVarSI tx
                      y' <- readTVarSI ty
                      return $ round x' == px && round y' == py
                   ) ts

-- | patch-here reports the patch under the turtle. 
patch_here :: STMorIO m => C Turtle _s' m [Patch]
patch_here = do
    (MkTurtle {xcor_ = x, ycor_ = y},_) <- Reader.ask
    x' <- lift $ readTVarSI x
    y' <- lift $ readTVarSI y
    patch x' y'


-- | Reports the agentset consisting of all turtles. 
turtles :: STMorIO m => C _s _s' m [Turtle]
turtles = lift $ do
    ts <- readTVarSI __turtles
    return $ IM.elems ts


-- | Reports the turtle with the given who number, or nobody if there is no such turtle. For breeded turtles you may also use the single breed form to refer to them. 
turtle :: STMorIO m => Int -> C _s _s' m [Turtle]
turtle n = lift $ do
    ts <- readTVarSI __turtles
    return $ maybe nobody return $ IM.lookup n ts


-- | This is a built-in turtle variable. It indicates the direction the turtle is facing. 
heading :: STMorIO m => C Turtle _s' m Double
heading = do
    (MkTurtle {heading_ = h},_) <- Reader.ask
    lift $ readTVarSI h


-- | This is a built-in turtle variable. It holds the current x coordinate of the turtle. 
xcor :: STMorIO m => C Turtle _s' m Double
xcor = do
    (MkTurtle {xcor_ = x},_) <- Reader.ask
    lift $ readTVarSI x

-- | This is a built-in turtle variable. It holds the current y coordinate of the turtle.
ycor :: STMorIO m => C Turtle _s' m Double
ycor = do
    (MkTurtle {ycor_ = y},_) <- Reader.ask
    lift $ readTVarSI y

pcolor :: STMorIO m => TurtlePatch s => C s _s' m Double
pcolor = do
    (s,_) <- Reader.ask
    (MkPatch {pcolor_ = tc}) <- patch_on_ s
    lift $ readTVarSI tc


plabel :: STMorIO m => TurtlePatch s => C s _s' m String
plabel = do
    (s,_) <- Reader.ask
    (MkPatch {plabel_ = tl}) <- patch_on_ s
    lift $ readTVarSI tl



-- | This is a built-in turtle variable. It holds the turtle's "who number" or ID number, an integer greater than or equal to zero. You cannot set this variable; a turtle's who number never changes. 
color :: STMorIO m => TurtleLink s => C s _s' m Double
color = do
    (s,_) <- Reader.ask
    lift $ readTVarSI (color_ s)


breed :: STMorIO m => TurtleLink s => C s _s' m String
breed = do
    (s,_) <- Reader.ask
    lift $ readTVarSI (breed_ s)


towards = undefined

-- -- | Reports the distance from this agent to the given turtle or patch. 
-- distance :: (TurtlePatch a, TurtlePatch s) => [a] -> C s _s' m Double

-- -- | Reports the distance from this agent to the point (xcor, ycor). 
-- distancexy :: TurtlePatch s => Double -> Double -> C s _s' m Double

-- -- | Reports the heading from this agent to the given agent. 
-- towards :: (TurtlePatch a, TurtlePatch s) => [a] -> C s _s' m Double

-- -- | Reports an agentset that includes only those agents from the original agentset whose distance from the caller is less than or equal to number. This can include the agent itself.
-- in_radius :: (TurtlePatch a, TurtlePatch s) => [a] -> Double -> C s _s' m [a]

-- | Given the who numbers of the endpoints, reports the link connecting the turtles. If there is no such link reports nobody. To refer to breeded links you must use the singular breed form with the endpoints. 
link :: STMorIO m => Int -> Int -> C _s _s' m [Link]
link f t = do
    ls <- lift $ readTVarSI __links
    return $ maybe nobody return $ M.lookup (f,t) ls


-- | Reports the agentset consisting of all links. 
links :: STMorIO m => C _s _s' m [Link]
links = lift $ do
    ls <- readTVarSI __links
    return $ nubBy checkForUndirected $ M.elems ls
        where
          checkForUndirected ((MkLink {end1_ = e1, end2_ = e2, directed_ = False})) ((MkLink {end1_ = e1', end2_ = e2', directed_ = False})) = e1 == e2' && e1' == e2
          checkForUndirected _ _ = False

readTurtle :: STMorIO m => Int -> C Turtle _s' m Double
readTurtle i = do
    (MkTurtle {tvars_ = pv},_) <- Reader.ask
    lift $ readTVarSI (pv ! i)


readPatch :: STMorIO m => TurtlePatch s => Int -> C s _s' m Double
readPatch i = do 
    (s,_) <- Reader.ask
    (MkPatch {pvars_ = pv}) <- patch_on_ s
    lift $ readTVarSI $ pv ! i

readLink :: STMorIO m => Int -> C Link _s' m Double
readLink i = do
    (MkLink {lvars_ = pv},_) <- Reader.ask
    lift $ readTVarSI (pv ! i)


-- timer :: C _s _s' m Double

-- reset_timer :: C _s _s' m ()

-- | Prints value in the Command Center, preceded by this agent, and followed by a carriage return.
--
-- HLogo-specific: There are no guarantees on which agent will be prioritized to write on the stdout. The only guarantee is that in case of show inside an 'atomic' transaction, no 'show' will be repeated if the transaction is retried. Compared to 'unsafe_show', the output is not mangled.
-- show :: (Player s, Show a) => a -> C s _s' m ()

-- | Prints value in the Command Center, followed by a carriage return. 
--
-- HLogo-specific: There are no guarantees on which agent will be prioritized to write on the stdout. The only guarantee is that in case of print inside an 'atomic' transaction, no 'print' will be repeated if the transaction is retried. Compared to 'unsafe_print', the output is not mangled.
-- print :: Show a => a -> C _s _s' m ()

-- | Reports the current value of the tick counter. The result is always a number and never negative. 
-- ticks :: C _s _s' m Double




--{-# WARNING towards "TODO: wrapping" #-}
--{-# WARNING timer "safe, but some might considered it unsafe with respect to STM, since it may poll the clock multiple times. The IO version of it is totally safe" #-}
--{-# WARNING reset_timer "safe, but some might considered it unsafe with respect to STM, since it may poll the clock multiple times. The IO version of it is totally safe" #-}
--{-# WARNING ticks "TODO: dynamic typing, integer or float" #-}

--   distance [PatchRef (x,y) _] = distancexy (fromIntegral x) (fromIntegral y)
--   distance [TurtleRef _ (MkTurtle {xcor_ = tx, ycor_ = ty})] = do
--     x <- lift $ readTVar tx
--     y <- lift $ readTVar ty
--     distancexy x y
--   distance _ = throw $ TypeException "single turtle or patch" Nobody

--   distancexy x' y' = do
--     (a,_) <- Reader.ask
--     (x,y) <- case a of
--               PatchRef (x,y) _ -> return (fromIntegral x, fromIntegral y)
--               TurtleRef _ (MkTurtle {xcor_ = tx, ycor_ = ty}) -> liftM2 (,) (lift $ readTVar tx) (lift $ readTVar ty)
--               _ -> throw $ ContextException "turtle or patch" a
--     return $ sqrt (deltaX x x' ^ 2 + 
--                 deltaY y y' ^ 2)
--     where
--       deltaX a1 a2 = if horizontal_wrap_ conf
--                      then min 
--                               (abs (a2 - a1))
--                               (abs (a2 - (fromIntegral $ max_pxcor_ conf) - a1 + (fromIntegral $ min_pxcor_ conf) + 1))
--                      else abs (a2 -a1)
--       deltaY a1 a2 = if vertical_wrap_ conf
--                      then min 
--                               (abs (a2 - a1)) 
--                               (abs (a2 - (fromIntegral $ max_pycor_ conf) - a1 + (fromIntegral $ min_pycor_ conf) + 1))
--                      else abs (a2 -a1)

--   towards a = do
--     (s,_) <- Reader.ask
--     (x1,y1) <- case s of
--               PatchRef (x,y) _ -> return (fromIntegral x, fromIntegral y)
--               TurtleRef _ (MkTurtle {xcor_ = tx, ycor_ = ty}) -> do
--                      x <- lift $ readTVar tx
--                      y <- lift $ readTVar ty
--                      return (x,y)
--               _ -> throw $ ContextException "turtle or patch" s
--     (x2,y2) <- case a of
--                 [PatchRef (x,y) _] -> return (fromIntegral x, fromIntegral y)
--                 [TurtleRef _ (MkTurtle {xcor_ = tx, ycor_ = ty})] -> do
--                      x <- lift $ readTVar tx
--                      y <- lift $ readTVar ty
--                      return (x,y)
--                 _ -> throw $ ContextException "turtle or patch" (head a)
--     let dx' = x2 - x1
--     let dy' = y2 - y1
--     return $ if dx' == 0
--               then
--                   if dy' > 0 
--                   then 0 
--                   else 180
--               else
--                   if dy' == 0
--                   then if dx' > 0 
--                        then 90 
--                        else 270
--                   else (270 + toDegrees (pi + atan2 (-dy') dx')) `mod_` 360

--   in_radius as n = do
--     (a,_) <- Reader.ask
--     (x, y) <- case a of
--                PatchRef (x,y) _ -> return (fromIntegral x, fromIntegral y)
--                TurtleRef _ (MkTurtle {xcor_ = tx, ycor_ = ty}) -> liftM2 (,) (lift $ readTVar tx) (lift $ readTVar ty)
--                _ -> throw $ ContextException "turtle or patch" a
--     filterM (\ (TurtleRef _ (MkTurtle {xcor_ = tx', ycor_ = ty'})) -> do 
--                x' <- lift $ readTVar tx'
--                y' <- lift $ readTVar ty'
--                return $ sqrt (delta x x' (fromIntegral (max_pxcor_ conf) :: Int) ^ (2::Int) + 
--                            delta y y' (fromIntegral (max_pycor_ conf) :: Int) ^ (2::Int)) <= n) as


  -- timer = lift $ do
  --     t <- readTVar __timer
  --     t' <- unsafeIOToSTM getCurrentTime
  --     return $ realToFrac (t' `diffUTCTime` t)              

  -- reset_timer = do
  --     t <- lift $ unsafeIOToSTM getCurrentTime
  --     lift $ writeTVar __timer t

show a = do
    -- ObserverRef _ -> lift $ unsafeIOToSTM $ putStrLn ("observer: " ++ Prelude.show a)
    (s,_) <- Reader.ask
    lift $ writeTQueue __printQueue $ Prelude.show s ++ ": " ++ Prelude.show a

print a = do
    -- ObserverRef _ -> lift $ unsafeIOToSTM $ Prelude.print a
    lift $ writeTQueue __printQueue $ Prelude.show a

ticks :: STMorIO m => C _s _s' m Double
ticks = lift $ readTVarSI __tick


--   distance [PatchRef (x,y) _] = distancexy (fromIntegral x) (fromIntegral y)
--   distance [TurtleRef _ (MkTurtle {xcor_ = tx, ycor_ = ty})] = do
--     x <- lift $ readTVarIO tx
--     y <- lift $ readTVarIO ty
--     distancexy x y
--   distance _ = throw $ ContextException "single turtle or patch" Nobody


--   distancexy x' y' = do
--     (a,_) <- Reader.ask
--     (x,y) <- case a of
--               PatchRef (x,y) _ -> return (fromIntegral x, fromIntegral y)
--               TurtleRef _ (MkTurtle {xcor_ = tx, ycor_ = ty}) -> liftM2 (,) (lift $ readTVarIO tx) (lift $ readTVarIO ty)
--               _ -> throw $ ContextException "turtle or patch" a
--     return $ sqrt (deltaX x x' ^ 2 + 
--                 deltaY y y' ^ 2)
--     where
--       deltaX a1 a2 = if horizontal_wrap_ conf
--                      then min 
--                               (abs (a2 - a1))
--                               (abs (a2 - fromIntegral (max_pxcor_ conf) - a1 + fromIntegral (min_pxcor_ conf) + 1))
--                      else abs (a2 -a1)
--       deltaY a1 a2 = if vertical_wrap_ conf
--                      then min 
--                               (abs (a2 - a1)) 
--                               (abs (a2 - fromIntegral (max_pycor_ conf) - a1 + fromIntegral (min_pycor_ conf) + 1))
--                      else abs (a2 -a1)

--   towards a = do
--     (s,_) <- Reader.ask
--     (x1,y1) <- case s of
--               PatchRef (x,y) _ -> return (fromIntegral x, fromIntegral y)
--               TurtleRef _ (MkTurtle {xcor_ = tx, ycor_ = ty}) -> do
--                      x <- lift $ readTVarIO tx
--                      y <- lift $ readTVarIO ty
--                      return (x,y)
--               _ -> throw $ ContextException "turtle or patch" s
--     (x2,y2) <- case a of
--                 [PatchRef (x,y) _] -> return (fromIntegral x, fromIntegral y)
--                 [TurtleRef _ (MkTurtle {xcor_ = tx, ycor_ = ty})] -> do
--                      x <- lift $ readTVarIO tx
--                      y <- lift $ readTVarIO ty
--                      return (x,y)
--                 _ -> throw $ ContextException "turtle or patch" (head a)
--     let dx' = x2 - x1
--     let dy' = y2 - y1
--     return $ if dx' == 0
--               then
--                   if dy' > 0 
--                   then 0 
--                   else 180
--               else
--                   if dy' == 0
--                   then if dx' > 0 
--                        then 90 
--                        else 270
--                   else (270 + toDegrees (pi + atan2 (-dy') dx')) `mod_` 360

--   in_radius as n = do
--     (a,_) <- Reader.ask
--     (x, y) <- case a of
--       PatchRef (x,y) _ -> return (fromIntegral x, fromIntegral y)
--       TurtleRef _ (MkTurtle {xcor_ = tx, ycor_ = ty}) -> liftM2 (,) (lift $ readTVarIO tx) (lift $ readTVarIO ty)
--       _ -> throw $ ContextException "turtle or patch" a
--     with (distancexy x y >>= \ d -> return $ d <= n) as


  -- timer = lift $ do
  --     t <- readTVarIO __timer
  --     t' <- getCurrentTime
  --     return $ realToFrac (t' `diffUTCTime` t)              

  -- reset_timer = do
  --     t <- lift $ getCurrentTime
  --     atomic $ lift $ writeTVar __timer t

  -- show a = do
  --   -- ObserverRef _ -> lift $ putStrLn ("observer: " ++ Prelude.show a)
  --   (s,_) <- Reader.ask
  --   atomic $ lift $ writeTQueue __printQueue $ Prelude.show s ++ ": " ++ Prelude.show a

  -- print a = do
  --   -- ObserverRef _ -> lift $ Prelude.print a
  --   atomic $ lift $ writeTQueue __printQueue $ Prelude.show a

  -- ticks = lift $ readIORef __tick


-- | Reports a shade of color proportional to the value of number. 
scale_color :: STMorIO m => Double -> C _s _s' m Double -> Double -> Double -> C _s _s' m Double
scale_color c v minArg maxArg = do
  let c' = findCentralColorNumber c - 5.0
  var <- v
  let perc | minArg > maxArg = if var < maxArg
                               then 1
                               else if var > minArg
                                    then 0
                                    else (minArg - var) / (minArg - maxArg)
           | var > maxArg =1 
           | var < minArg = 0
           | otherwise = (var - minArg) / (maxArg - minArg)
  return $ c' + let perc' = perc * 10
                in if perc' >= 9.9999
                   then 9.9999
                   else if perc' < 0
                        then 0
                        else perc'
    where
      -- | Internal
      findCentralColorNumber :: Double -> Double
      findCentralColorNumber c = (fromIntegral (truncate (modulateDouble c / 10)) + 0.5) * 10
      -- | Internal
      -- It has bug with mod_ truncate
      modulateDouble :: Double -> Double
      modulateDouble c = runIdentity $
        if c < 0 || c >= maxColor
        then do
          let c' = c `mod_` truncate maxColor
          if c'<0
            then do
              let c'' = c' + maxColor
              if c'' >= maxColor
               then return 139.9999999999999
               else return c''
            else return c'
        else return c
             where maxColor = 140.0

-- | Tells each patch to give equal shares of (number * 100) percent of the value of patch-variable to its eight neighboring patches. number should be between 0 and 1. Regardless of topology the sum of patch-variable will be conserved across the world. (If a patch has fewer than eight neighbors, each neighbor still gets an eighth share; the patch keeps any leftover shares.) 
-- can be done better, in a single sequential atomic
-- diffuse :: CSTM Double -> (Double -> CSTM ()) -> Double -> C Observer () IO ()
-- diffuse gettervar settervar perc = ask (do
--                           ns <- neighbors
--                           cns <- count ns
--                           g <- atomic gettervar
--                           let pg = g * perc / 8
--                           ask (atomic (do
--                                ng <- gettervar
--                                settervar (ng + pg))) ns
--                           atomic $ settervar (g - (pg * fromIntegral cns))
--                         ) =<< patches

-- Specialization trick to reduce the cost of using a class (STMorIO)
-- The downside is executable with bigger code

{-# SPECIALIZE  turtles_here :: TurtlePatch s => C s _s' STM [Turtle] #-}
{-# SPECIALIZE  turtles_here :: TurtlePatch s => C s _s' IO [Turtle] #-}
{-# SPECIALIZE  turtles_at :: TurtlePatch s => Double -> Double -> C s _s' STM [Turtle] #-}
{-# SPECIALIZE  turtles_at :: TurtlePatch s => Double -> Double -> C s _s' IO [Turtle] #-}
{-# SPECIALIZE  patch_here :: C Turtle _s' STM [Patch] #-}
{-# SPECIALIZE  patch_here :: C Turtle _s' IO [Patch] #-}
{-# SPECIALIZE  patches :: C _s _s' STM [Patch] #-}
{-# SPECIALIZE  patches :: C _s _s' IO [Patch] #-}
{-# SPECIALIZE  patch :: Double -> Double -> C _s _s' STM [Patch] #-}
{-# SPECIALIZE  patch :: Double -> Double -> C _s _s' IO [Patch] #-}
{-# SPECIALIZE  turtles :: C _s _s' STM [Turtle] #-}
{-# SPECIALIZE  turtles :: C _s _s' IO [Turtle] #-}
{-# SPECIALIZE  turtle :: Int -> C _s _s' STM [Turtle] #-}
{-# SPECIALIZE  turtle :: Int -> C _s _s' IO [Turtle] #-}
{-# SPECIALIZE  heading :: C Turtle _s' STM Double #-}
{-# SPECIALIZE  heading :: C Turtle _s' IO Double #-}
{-# SPECIALIZE  xcor :: C Turtle _s' STM Double #-}
{-# SPECIALIZE  xcor :: C Turtle _s' IO Double #-}
{-# SPECIALIZE  ycor :: C Turtle _s' STM Double #-}
{-# SPECIALIZE  ycor :: C Turtle _s' IO Double #-}
{-# SPECIALIZE  pcolor :: C Turtle _s' STM Double #-}
{-# SPECIALIZE  pcolor :: C Turtle _s' IO Double #-}
{-# SPECIALIZE  pcolor :: C Patch _s' STM Double #-}
{-# SPECIALIZE  pcolor :: C Patch _s' IO Double #-}
{-# SPECIALIZE  color :: TurtleLink s => C s _s' STM Double #-}
{-# SPECIALIZE  color :: TurtleLink s => C s _s' IO Double #-}
{-# SPECIALIZE  breed :: TurtleLink s => C s _s' STM String #-}
{-# SPECIALIZE  breed :: TurtleLink s => C s _s' IO String #-}
-- {-# SPECIALIZE  distance :: [AgentRef] -> CSTM Double #-}
-- {-# SPECIALIZE  distance :: [AgentRef] -> CIO Double #-}
-- {-# SPECIALIZE  distancexy :: Double -> Double -> CSTM Double #-}
-- {-# SPECIALIZE  distancexy :: Double -> Double -> CIO Double #-}
-- {-# SPECIALIZE  towards :: [AgentRef] -> CSTM Double #-}
-- {-# SPECIALIZE  towards :: [AgentRef] -> CIO Double #-}
-- {-# SPECIALIZE  in_radius :: [AgentRef] -> Double -> CSTM [AgentRef] #-}
-- {-# SPECIALIZE  in_radius :: [AgentRef] -> Double -> CIO [AgentRef] #-}
{-# SPECIALIZE   link :: Int -> Int -> C _s _s' STM [Link] #-}
{-# SPECIALIZE   link :: Int -> Int -> C _s _s' IO [Link] #-}
{-# SPECIALIZE  links :: C _s _s' STM [Link] #-}
{-# SPECIALIZE  links :: C _s _s' IO [Link] #-}
{-# SPECIALIZE  readTurtle :: Int -> C Turtle _s' STM Double #-}
{-# SPECIALIZE  readTurtle :: Int -> C Turtle _s' IO Double #-}
{-# SPECIALIZE  readPatch :: TurtlePatch s => Int -> C s _s' STM Double #-}
{-# SPECIALIZE  readPatch :: TurtlePatch s => Int -> C s _s' IO Double #-}
{-# SPECIALIZE  readLink :: Int -> C Link _s' STM Double #-}
{-# SPECIALIZE  readLink :: Int -> C Link _s' IO Double #-}
--{-# SPECIALIZE  timer :: C _s _s' STM Double #-}
--{-# SPECIALIZE  timer :: C _s _s' IO Double #-}
--{-# SPECIALIZE  reset_timer :: C _s _s' STM () #-}
--{-# SPECIALIZE  reset_timer :: C _s _s' IO () #-}
--{-# SPECIALIZE  show :: (Player s, Show a) => a -> C s _s' STM () #-}
--{-# SPECIALIZE  show :: (Player s, Show a) => a -> C s _s' IO () #-}
--{-# SPECIALIZE  print :: Show a => a -> C _s _s' STM () #-}
--{-# SPECIALIZE  print :: Show a => a -> C _s _s' IO () #-}
{-# SPECIALIZE  ticks :: C _s _s' STM Double #-}
{-# SPECIALIZE  ticks :: C _s _s' IO Double #-}




class Monad m => STMorIO m where
    readTVarSI :: TVar a -> m a

instance STMorIO STM where
    readTVarSI = readTVar

instance STMorIO IO where
    readTVarSI = readTVarIO
