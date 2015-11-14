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
                           self, myself, other, count, distance, nobody, distancexy, towards, allp, at_points, towardsxy, in_radius, in_cone, every, wait, is_agentp, carefully, is_agentsetp, die, 

                           -- * Turtle related
                           turtles_here, turtles_at, turtles_on, jump, setxy, forward, fd, back, bk, turtles, turtle, turtle_set, face, xcor, set_breed, with_breed, set_color, with_color, set_label_color, with_label_color, with_label, set_xcor, heading, set_heading, with_heading,  ycor, set_ycor, who, color, breed, dx, dy, home, right, rt, left, lt, downhill, downhill4, hide_turtle, ht, show_turtle, st, pen_down, pd, pen_up, pu, pen_erase, pe, no_turtles, is_turtlep, is_turtle_setp, hatch, move_to, set_size, with_size, with_shape,

                           -- * Patch related
                           patch_at, patch_here, patch_ahead, patches, patch, patch_set, can_movep, no_patches, is_patchp, is_patch_setp, pxcor, pycor,pcolor,  neighbors, neighbors4, set_plabel, with_plabel, set_pcolor, with_pcolor, with_plabel_color, diffuse,

                           -- * Link related
                           hide_link, show_link, is_linkp, is_directed_linkp, is_undirected_linkp, is_link_setp, link_length, link, links, link_with, in_link_from, out_link_to, my_links, my_out_links, my_in_links, no_links, tie, untie, link_set, end1, end2, 

                           -- * Random related
                           random_xcor, unsafe_random_xcor, random_ycor, unsafe_random_ycor, random_pxcor, unsafe_random_pxcor, random_pycor, unsafe_random_pycor, random, unsafe_random, random_float, unsafe_random_float, new_seed, random_seed, unsafe_random_seed, random_exponential, random_gamma, random_normal, random_poisson,

                           -- * Color
                           black, white, gray, red, orange, brown, yellow, green, lime, turquoise, cyan, sky, blue, violet, magenta, pink, scale_color, extract_rgb, approximate_rgb,

                           -- * List related
                           sum, anyp, item, one_of, min_one_of, max_one_of, unsafe_one_of, unsafe_n_of, remove, remove_item, replace_item, shuffle, unsafe_shuffle, sublist, substring, n_of, butfirst, butlast, emptyp, first, foreach, fput, last, length, list, lput, map, memberp, position, reduce, remove_duplicates, reverse, sentence, sort_, sort_by, sort_on, max_, min_,n_values, is_listp, is_stringp, word,

                           -- * Math
                           xor, e, exp, pi, cos_, sin_, tan_, mod_, acos_, asin_, atan_, int, log_, ln, mean, median, modes, variance, standard_deviation, subtract_headings, abs_, floor, ceiling, remainder, round, sqrt,  is_numberp,

                           -- * Misc
                           patch_size, max_pxcor, max_pycor, min_pxcor, min_pycor, world_width, world_height, clear_all_plots, clear_drawing, cd, clear_output, clear_turtles, ct, clear_patches, cp, clear_links, clear_ticks, reset_ticks, tick, tick_advance, ticks, histogram, repeat_, report, loop, stop, while, STMorIO, readGlobal, readTurtle, readPatch, readLink, stats_stm,

                           -- * Input/Output
                           show, unsafe_show, print, unsafe_print, read_from_string, timer, reset_timer,

                           -- * IO Operations
                           atomic, ask, of_, with, snapshot


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
import qualified Control.Concurrent.Thread as Thread (forkIO, result)
import qualified Control.Concurrent.Thread.Group as ThreadG (forkIO, wait)
import Data.List
import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict as M
import Data.Array
import Control.Applicative
import System.Random hiding (random, split)
import Data.Function
import Data.Typeable
import Control.Monad (forM_, liftM, liftM2, filterM, forever, when)
import Data.Word (Word8)
import GHC.Conc (numCapabilities)
import qualified Data.Traversable as T (mapM)
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

{-# DEPRECATED unsafe_random_xcor, unsafe_random_ycor, unsafe_random_pxcor, unsafe_random_pycor, unsafe_random, unsafe_random_float, unsafe_random_seed, unsafe_one_of, unsafe_n_of, unsafe_shuffle "Uses slower (but safe) global random generator" #-}

{-# SPECIALIZE self :: CSTM [AgentRef] #-}
{-# SPECIALIZE self :: CIO [AgentRef] #-}
-- |  Reports this turtle or patch. 
self :: Monad m => C m [AgentRef] -- ^ returns a list (set) of agentrefs to be compatible with the 'turtle-set' function
self = do
  (_, a, _, _) <- Reader.ask
  case a of
    TurtleRef _ _ -> return [a]
    PatchRef _ _ -> return [a]
    LinkRef _ _ -> return [a]
    _ -> throw (ContextException "agent" a)


{-# SPECIALIZE myself :: CSTM [AgentRef] #-}
{-# SPECIALIZE myself :: CIO [AgentRef] #-}
-- | "self" and "myself" are very different. "self" is simple; it means "me". "myself" means "the turtle or patch who asked me to do what I'm doing right now."
-- When an agent has been asked to run some code, using myself in that code reports the agent (turtle or patch) that did the asking. 
-- NB: Implemented for ask, of, with
myself :: (Monad m) => C m [AgentRef]
myself = do
  (_,s,_,m) <- Reader.ask
  return $ case s of
             TurtleRef _ _ -> [m]
             PatchRef _ _ -> [m]
             LinkRef _ _ -> [m]
             _ -> throw (ContextException "agent" s)

{-# SPECIALIZE other :: [AgentRef] -> CSTM [AgentRef] #-}
{-# SPECIALIZE other :: [AgentRef] -> CIO [AgentRef] #-}
-- |  Reports an agentset which is the same as the input agentset but omits this agent. 
other :: Monad m => [AgentRef] -> C m [AgentRef]
other as = do
  [s] <- self
  return $ delete s as








                           

{-# WARNING carefully "TODO" #-}
-- | Runs commands1. If a runtime error occurs inside commands1, NetLogo won't stop and alert the user that an error occurred. It will suppress the error and run commands2 instead. 
carefully :: CSTM a -> CSTM a -> CSTM a
carefully c c' = catch c (\ ex -> let _ = (ex :: SomeException) in c')

{-# SPECIALIZE patch_at :: Double -> Double -> CSTM [AgentRef] #-}
{-# SPECIALIZE patch_at :: Double -> Double -> CIO [AgentRef] #-}
-- | Reports the patch at (dx, dy) from the caller, that is, the patch containing the point dx east and dy patches north of this agent. 
patch_at :: STMorIO m => Double -> Double ->  C m [AgentRef]
patch_at x y = do
  ( _, a, _, _) <- Reader.ask
  case a of
    PatchRef (px, py) _ -> patch (fromIntegral px) (fromIntegral py)
    TurtleRef _ _ -> do
                 [PatchRef (px, py) _] <- patch_here
                 patch (fromIntegral px + x) (fromIntegral py +y)
    _ -> throw $ ContextException "turtle or patch" a
                 


{-# SPECIALIZE patch_ahead :: Double -> CSTM [AgentRef] #-}
{-# SPECIALIZE patch_ahead :: Double -> CIO [AgentRef] #-}
-- | Reports the single patch that is the given distance "ahead" of this turtle, that is, along the turtle's current heading. 
patch_ahead :: STMorIO m => Double -> C m [AgentRef]
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

{-# SPECIALIZE count :: [AgentRef] -> CSTM Int #-}
{-# SPECIALIZE count :: [AgentRef] -> CIO Int #-}
--count :: (Monad m, Num a) => [AgentRef] -> C m a
-- | Reports the number of agents in the given agentset. 
count :: Monad m => [AgentRef] -> C m Int
count [Nobody] = throw $ TypeException "agent" Nobody
count as = return $ length as

{-# SPECIALIZE anyp :: [AgentRef] -> CSTM Bool #-}
{-# SPECIALIZE anyp :: [AgentRef] -> CIO Bool #-}
-- | Reports true if the given agentset is non-empty, false otherwise. 
anyp :: Monad m => [AgentRef] -> C m Bool
anyp [Nobody] = throw $ TypeException "agent" Nobody
anyp as = return $ not $ null as

allp :: CIO Bool -> [AgentRef] -> CIO Bool
allp _ [] = return True
allp r as = do
  res <- with r as
  return $ length as == length res

-- | The turtle moves forward by number units all at once (rather than one step at a time as with the forward command). 
jump :: Double -> CSTM ()
jump n = do
  (_, a, _, _) <- Reader.ask
  case a of
    TurtleRef _ (MkTurtle {xcor_ = tx, ycor_ = ty, heading_ = th}) -> do
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
    _ -> throw $ ContextException "turtle" a


-- | The turtle sets its x-coordinate to x and its y-coordinate to y. 
setxy :: Double -> Double -> CSTM ()
setxy x' y' = do
  (_, a, _, _) <- Reader.ask
  case a of
    TurtleRef _ (MkTurtle {xcor_ = tx, ycor_ = ty}) -> do
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
    _ -> throw $ ContextException "turtle" a


-- | The turtle moves forward by number steps, one step at a time. (If number is negative, the turtle moves backward.) 
forward :: Double -> CSTM ()
forward n | n == 0 = do
                        (_, a, _, _) <- Reader.ask
                        case a of
                          TurtleRef _ _ -> return ()
                          _ -> throw $ ContextException "turtle" a
          | n > 1 = jump 1 >> forward (n-1)
          | n < -1 = jump (-1) >> forward (n+1)
          | (0 < n && n <= 1) || (-1 <= n && n < 0) = jump n
          | otherwise = throw DevException
 
{-# INLINE fd #-}
-- | alias for 'forward'
fd :: Double -> CSTM ()
fd = forward

-- | The turtle moves backward by number steps. (If number is negative, the turtle moves forward.) 
{-# INLINE back #-}
back :: Double -> CSTM ()
back n = forward (-n)
{-# INLINE bk #-}
-- | alias for 'back'
bk :: Double -> CSTM ()
bk = back

-- | As it is right now, if an agent holds a past reference to a turtle, it can still modify it and ask it to do sth. 
-- The only guarantee is that the __next__ 'turtles','turtles_at','turtles_here','turtles_on'... etc
-- will not return this dead agent.
die :: CSTM ()
die = do
 (tw,a,_,_) <- Reader.ask
 case a of
   TurtleRef t _ -> do
          (MkWorld ps ts ls) <- lift $ readTVar tw
          lift $ writeTVar tw $ MkWorld ps (IM.delete t ts) ls
   LinkRef (e1,e2) (MkLink {directed_ = d}) -> do
          (MkWorld ps ts ls) <- lift $ readTVar tw
          lift $ writeTVar tw $ MkWorld ps ts (M.delete (e1,e2) 
                                                (if d -- is directed
                                                then ls
                                                else M.delete (e2,e1) ls
                                                )
                                              )
   _ -> throw $ ContextException "turtle or patch" a






{-# SPECIALIZE turtle_set :: [CSTM [AgentRef]] -> CSTM [AgentRef] #-}
{-# SPECIALIZE turtle_set :: [CIO [AgentRef]] -> CIO [AgentRef] #-}
-- | Reports an agentset containing all of the turtles anywhere in any of the inputs.
-- | NB: HLogo no support for nested turtle_set concatenation/flattening
turtle_set :: Monad m => [C m [AgentRef]] -> C m [AgentRef]
turtle_set ts = liftM (foldr (\ x acc -> 
                                  if x == Nobody -- filter Nobody
                                  then acc
                                  else case x of -- type check
                                         TurtleRef _ _ -> if x `elem` acc -- nub
                                                         then acc
                                                         else x:acc
                                         _ -> throw $ TypeException "turtle" x
                             ) [] . concat) (sequence ts)

{-# SPECIALIZE patch_set :: [CSTM [AgentRef]] -> CSTM [AgentRef] #-}
{-# SPECIALIZE patch_set :: [CIO [AgentRef]] -> CIO [AgentRef] #-}
-- | Reports an agentset containing all of the patches anywhere in any of the inputs.
-- | NB: HLogo no support for nested patch_set concatenation/flattening
patch_set :: Monad m => [C m [AgentRef]] -> C m [AgentRef]
patch_set ts = liftM (foldr (\ x acc -> 
                                 if x == Nobody -- filter Nobody
                                 then acc
                                 else case x of -- type check
                                        PatchRef _ _ -> if x `elem` acc -- nub
                                                       then acc
                                                       else x:acc
                                        _ -> throw $ TypeException "patch" x
                            ) [] . concat) (sequence ts)


{-# SPECIALIZE can_movep :: Double -> CSTM Bool #-}
{-# SPECIALIZE can_movep :: Double -> CIO Bool #-}
-- | Reports true if this turtle can move distance in the direction it is facing without violating the topology; reports false otherwise. 
can_movep :: STMorIO m => Double -> C m Bool
can_movep n = liftM ( /= [Nobody]) $ patch_ahead n


set_heading :: Double -> CSTM ()
set_heading v = do
  (_,a, _, _) <- Reader.ask
  case a of
    TurtleRef _ t -> lift $ writeTVar (heading_ t) v
    _ -> throw $ ContextException "turtle" a

{-# SPECIALIZE pxcor :: CSTM Int #-}
{-# SPECIALIZE pxcor :: CIO Int #-}
-- pxcor :: (Monad m, Num a) => C m a
-- |These are built-in patch variables. They hold the x and y coordinate of the patch. They are always integers. You cannot set these variables, because patches don't move. 
pxcor :: (Monad m) => C m Int
pxcor = do
  (_,a, _, _) <- Reader.ask
  case a of
    PatchRef _ (MkPatch {pxcor_ = x}) -> return x
    _ -> throw $ ContextException "patch" a

{-# SPECIALIZE pycor :: CSTM Int #-}
{-# SPECIALIZE pycor :: CIO Int #-}
-- pycor :: (Monad m, Num a) => C m a
-- | These are built-in patch variables. They hold the x and y coordinate of the patch. They are always integers. You cannot set these variables, because patches don't move. 
pycor :: (Monad m) => C m Int
pycor = do
  (_,a, _, _) <- Reader.ask
  case a of
    PatchRef _ (MkPatch {pycor_ = y}) -> return y
    _ -> throw $ ContextException "patch" a

set_plabel :: String -> CSTM ()
set_plabel s = do
  (_,a,_,_) <- Reader.ask
  case a of
    PatchRef _ (MkPatch {plabel_ = p}) -> lift $ writeTVar p s
    _ -> throw $ ContextException "patch" a

set_pcolor :: Double -> CSTM ()
set_pcolor s = do
  (_,a,_,_) <- Reader.ask
  case a of
    PatchRef _ (MkPatch {pcolor_ = tc}) -> lift $ writeTVar tc s
    TurtleRef _ _ -> do
                 [PatchRef _ (MkPatch {pcolor_ = tc})] <- patch_here
                 lift $ writeTVar tc s
    _ -> throw $ ContextException "turtle or patch" a

set_breed :: String -> CSTM ()
set_breed v = do
  (_,a, _, _) <- Reader.ask
  case a of
    TurtleRef _ t -> lift $ writeTVar (breed_ t) v
    _ -> throw $ ContextException "turtle" a

set_color :: Double -> CSTM ()
set_color v = do
  (_,a, _, _) <- Reader.ask
  case a of
    TurtleRef _ t -> lift $ writeTVar (color_ t) v
    LinkRef _ (MkLink {lcolor_ = c}) -> lift $ writeTVar c v
    _ -> throw $ ContextException "turtle or link" a

set_label_color :: Double -> CSTM ()
set_label_color v = do
  (_,a, _, _) <- Reader.ask
  case a of
    TurtleRef _ t -> lift $ writeTVar (label_color_ t) v
    LinkRef _ (MkLink {llabel_color_ = c}) -> lift $ writeTVar c v
    _ -> throw $ ContextException "turtle or link" a


set_xcor :: Double -> CSTM ()
set_xcor x' = do
  (_,a, _, _) <- Reader.ask
  case a of
    TurtleRef _ (MkTurtle {xcor_ = tx}) -> do
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
    _ -> throw $ ContextException "turtle" a


set_size :: Double -> CSTM ()
set_size v = do
  (_,a, _, _) <- Reader.ask
  case a of
    TurtleRef _ t -> lift $ writeTVar (size_ t) v
    _ -> throw $ ContextException "turtle" a





set_ycor :: Double -> CSTM ()
set_ycor y' = do
  (_,a, _, _) <- Reader.ask
  case a of
    TurtleRef _ (MkTurtle {ycor_ = ty}) -> do
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
    _ -> throw $ ContextException "turtle" a

{-# SPECIALIZE who :: CSTM Int #-}
{-# SPECIALIZE who :: CIO Int #-}
-- | This is a built-in turtle variable. It holds the turtle's "who number" or ID number, an integer greater than or equal to zero. You cannot set this variable; a turtle's who number never changes. 
who :: Monad m => C m Int
who = do
  (_,a, _, _) <- Reader.ask
  case a of
    TurtleRef i _ -> return i
    _ -> throw $ ContextException "turtle" a


{-# SPECIALIZE dx :: CSTM Double #-}
{-# SPECIALIZE dx :: CIO Double #-}
-- | Reports the x-increment (the amount by which the turtle's xcor would change) if the turtle were to take one step forward in its current heading. 
dx :: STMorIO m => C m Double
dx = liftM sin_ heading

{-# SPECIALIZE dy :: CSTM Double #-}
{-# SPECIALIZE dy :: CIO Double #-}
-- | Reports the y-increment (the amount by which the turtle's ycor would change) if the turtle were to take one step forward in its current heading. 
dy :: STMorIO m => C m Double
dy = liftM cos_ heading

-- | Reports a number suitable for seeding the random number generator.
-- The numbers reported by new-seed are based on the current date and time in milliseconds. 
-- Unlike NetLogo's new-seed, HLogo may report the same number twice in succession.
--
-- NB: taken from Haskell's random library
new_seed :: CSTM Int
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

random_seed :: Int -> CSTM ()
random_seed i = do
  (_,s,_,_) <- Reader.ask
  let g = case s of
            ObserverRef tg -> tg
            TurtleRef _ t -> tgen t
            PatchRef _ p -> pgen p
            LinkRef _ l -> lgen l
            Nobody -> throw DevException
  lift $ writeTVar g (mkStdGen i)
                
-- | Reports a random floating point number from the allowable range of turtle coordinates along the given axis, x . 
random_xcor :: CSTM Double
random_xcor = do
  (_,s,_,_) <- Reader.ask
  let g = case s of
            ObserverRef tg -> tg
            TurtleRef _ t -> tgen t
            PatchRef _ p -> pgen p
            LinkRef _ l -> lgen l
            Nobody -> throw DevException
  gen <- lift $ readTVar g
  let (v, gen') = randomR (fromIntegral (min_pxcor_ conf) :: Double, fromIntegral $ max_pxcor_ conf) gen
  lift $ writeTVar g gen'
  return v

-- | Reports a random floating point number from the allowable range of turtle coordinates along the given axis, y. 
random_ycor :: CSTM Double
random_ycor = do
  (_,s,_,_) <- Reader.ask
  let g = case s of
            ObserverRef tg -> tg
            TurtleRef _ t -> tgen t
            PatchRef _ p -> pgen p
            LinkRef _ l -> lgen l
            Nobody -> throw DevException
  gen <- lift $ readTVar g
  let (v, gen') = randomR (fromIntegral (min_pycor_ conf) :: Double, fromIntegral $ max_pycor_ conf) gen
  lift $ writeTVar g gen'
  return v

-- | Reports a random integer ranging from min-pxcor to max-pxcor inclusive. 
random_pxcor :: CSTM Int
random_pxcor = do
  (_,s,_,_) <- Reader.ask
  let g = case s of
            ObserverRef tg -> tg
            TurtleRef _ t -> tgen t
            PatchRef _ p -> pgen p
            LinkRef _ l -> lgen l
            Nobody -> throw DevException
  gen <- lift $ readTVar g
  let (v, gen') = randomR (min_pxcor_ conf, max_pxcor_ conf) gen
  lift $ writeTVar g gen'
  return v

-- | Reports a random integer ranging from min-pycor to max-pycor inclusive. 
random_pycor :: CSTM Int
random_pycor = do
  (_,s,_,_) <- Reader.ask
  let g = case s of
            ObserverRef tg -> tg
            TurtleRef _ t -> tgen t
            PatchRef _ p -> pgen p
            LinkRef _ l -> lgen l
            Nobody -> throw DevException
  gen <- lift $ readTVar g
  let (v, gen') = randomR (min_pycor_ conf, max_pycor_ conf) gen
  lift $ writeTVar g gen'
  return v

-- | If number is positive, reports a random integer greater than or equal to 0, but strictly less than number.
-- If number is negative, reports a random integer less than or equal to 0, but strictly greater than number.
-- If number is zero, the result is always 0 as well. 
random               :: (Random a , Eq a, Ord a, Num a) => a -> CSTM a
random x | x == 0     = return 0
         | otherwise = do
  (_,s,_,_) <- Reader.ask
  let ts = case s of
            ObserverRef tg -> tg
            TurtleRef _ t -> tgen t
            PatchRef _ p -> pgen p
            LinkRef _ l -> lgen l
            Nobody -> throw DevException
  gen <- lift $ readTVar ts
  let (v, gen') = randomR (if x < 0 then (x, 0) else (0,x)) gen
  lift $ writeTVar ts gen'
  return v

-- |  If number is positive, reports a random floating point number greater than or equal to 0 but strictly less than number.
-- If number is negative, reports a random floating point number less than or equal to 0, but strictly greater than number.
-- If number is zero, the result is always 0. 
random_float               :: Double -> CSTM Double
random_float x | x == 0     = return 0
         | otherwise = do
  (_,s,_,_) <- Reader.ask
  let ts = case s of
            ObserverRef tg -> tg
            TurtleRef _ t -> tgen t
            PatchRef _ p -> pgen p
            LinkRef _ l -> lgen l
            Nobody -> throw DevException
  gen <- lift $ readTVar ts
  let (v, gen') = randomR (if x < 0 then (x, 0) else (0,x)) gen
  lift $ writeTVar ts gen'
  return v

-- | This turtle moves to the origin (0,0). Equivalent to setxy 0 0. 
home :: CSTM ()
home = setxy 0 0

-- | The turtle turns right by number degrees. (If number is negative, it turns left.) 
right :: Double -> CSTM ()
right n = do
  (_, s, _, _) <- Reader.ask
  case s of
    TurtleRef _ t -> lift $ modifyTVar' (heading_ t) (\ h -> mod_ (h+n) 360)
    _ -> throw $ ContextException "turtle" s
{-# INLINE rt #-}
-- | alias for 'right'
rt :: Double -> CSTM ()
rt = right

-- | The turtle turns left by number degrees. (If number is negative, it turns right.) 
left :: Double -> CSTM ()
left n = right (-n)

{-# INLINE lt #-}
-- | alias for 'left'
lt :: Double -> CSTM ()
lt = left

{-# WARNING delta "TODO: there is some problem here, an argument is ignored" #-}
-- | Internal
delta :: (Num a, Ord a) => a -> a -> t -> a
delta a1 a2 _aboundary =
    min (abs (a2 - a1)) (abs (a2 + a1) + 1)


{-# SPECIALIZE nobody :: CSTM [AgentRef] #-}
{-# SPECIALIZE nobody :: CIO [AgentRef] #-}
-- | This is a special value which some primitives such as turtle, one-of, max-one-of, etc. report to indicate that no agent was found. Also, when a turtle dies, it becomes equal to nobody. 
--
-- It can be returned from all primitives that normally return 1 agent. It can also be returned from a turtle reference that got died or the 'turtle' primitive to a dead agent,, like implicitly nullifying the agent.
nobody :: Monad m => m [AgentRef]
nobody = return [Nobody]

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
face :: [AgentRef] -> CSTM ()
face a = set_heading =<< towards a


{-# WARNING towardsxy "TODO" #-}
-- | Reports the heading from the turtle or patch towards the point (x,y). 
towardsxy :: t
towardsxy = todo


-- | The turtle makes itself invisible. 
hide_turtle :: CSTM ()
hide_turtle = do
  (_,a, _, _) <- Reader.ask
  case a of
    TurtleRef _ (MkTurtle {hiddenp_ = th}) -> lift $ writeTVar th True
    _ -> throw $ ContextException "turtle" a

{-# INLINE ht #-}
-- | alias for 'hide_turtle'
ht :: CSTM ()
ht = hide_turtle

-- | The turtle becomes visible again. 
show_turtle :: CSTM ()
show_turtle = do
  (_,a, _, _) <- Reader.ask
  case a of
    TurtleRef _ (MkTurtle {hiddenp_ = th}) -> lift $ writeTVar th False
    _ -> throw $ ContextException "turtle" a

{-# INLINE st #-}
-- | alias for 'show_turtle'
st :: CSTM ()
st = show_turtle

-- | The turtle changes modes between drawing lines, removing lines or neither. 
pen_down :: CSTM ()
pen_down = do
  (_,a, _, _) <- Reader.ask
  case a of
    TurtleRef _ (MkTurtle {pen_mode_ = tp}) -> lift $ writeTVar tp Down
    _ -> throw $ ContextException "turtle" a

{-# INLINE pd #-}
-- | alias for 'pen_down'
pd :: CSTM ()
pd = pen_down

-- | The turtle changes modes between drawing lines, removing lines or neither. 
pen_up :: CSTM ()
pen_up = do
  (_,a, _, _) <- Reader.ask
  case a of
    TurtleRef _ (MkTurtle {pen_mode_ = tp}) -> lift $ writeTVar tp Up
    _ -> throw $ ContextException "turtle" a

{-# INLINE pu #-}
-- | alias for 'pen_up'
pu :: CSTM ()
pu = pen_up

pen_erase :: CSTM ()
-- | The turtle changes modes between drawing lines, removing lines or neither. 
pen_erase = do
  (_,a, _, _) <- Reader.ask
  case a of
    TurtleRef _ (MkTurtle {pen_mode_ = tp}) -> lift $ writeTVar tp Erase
    _ -> throw $ ContextException "turtle" a

{-# INLINE pe #-}
-- | alias for 'pen_erase'
pe :: CSTM ()
pe = pen_erase







-- | This reporter lets you give a turtle a "cone of vision" in front of itself. 
in_cone :: t
in_cone = todo




{-# SPECIALIZE no_turtles :: CSTM [AgentRef] #-}
{-# SPECIALIZE no_turtles :: CIO [AgentRef] #-}
-- | Reports an empty turtle agentset. 
no_turtles :: (Monad m) => C m [AgentRef]
no_turtles = return []

{-# SPECIALIZE no_patches :: CSTM [AgentRef] #-}
{-# SPECIALIZE no_patches :: CIO [AgentRef] #-}
-- | Reports an empty patch agentset. 
no_patches :: (Monad m) => C m [AgentRef]
no_patches = return []

-- | Reports true if either boolean1 or boolean2 is true, but not when both are true. 
xor :: Bool -> Bool -> Bool
xor p q = (p || q) && not (p && q)


{-# SPECIALIZE patch_size :: CSTM Int #-}
{-# SPECIALIZE patch_size :: CIO Int #-}
patch_size :: Monad m => C m Int
patch_size = return $ patch_size_ conf

{-# SPECIALIZE max_pxcor :: CSTM Int #-}
{-# SPECIALIZE max_pxcor :: CIO Int #-}
-- | This reporter gives the maximum x-coordinate for patches, which determines the size of the world. 
max_pxcor :: Monad m => C m Int
max_pxcor = return $ max_pxcor_ conf

{-# SPECIALIZE max_pycor :: CSTM Int #-}
{-# SPECIALIZE max_pycor :: CIO Int #-}
-- | This reporter gives the maximum y-coordinate for patches, which determines the size of the world. 
max_pycor :: (Monad m) => C m Int
max_pycor = return $ max_pycor_ conf

{-# SPECIALIZE min_pxcor :: CSTM Int #-}
{-# SPECIALIZE min_pxcor :: CIO Int #-}
-- | This reporter gives the minimum x-coordinate for patches, which determines the size of the world. 
min_pxcor :: (Monad m ) => C m Int
min_pxcor = return $ min_pxcor_ conf

{-# SPECIALIZE min_pycor :: CSTM Int #-}
{-# SPECIALIZE min_pycor :: CIO Int #-}
-- | This reporter gives the maximum y-coordinate for patches, which determines the size of the world. 
min_pycor :: (Monad m) => C m Int
min_pycor = return $ min_pycor_ conf

{-# SPECIALIZE world_width :: CSTM Int #-}
{-# SPECIALIZE world_width :: CIO Int #-}
-- | This reporter gives the total width of the NetLogo world. 
world_width :: (Monad m) => C m Int
world_width = return $ max_pxcor_ conf - min_pxcor_ conf + 1

{-# SPECIALIZE world_height :: CSTM Int #-}
{-# SPECIALIZE world_height :: CIO Int #-}
-- | This reporter gives the total height of the NetLogo world. 
world_height :: (Monad m) => C m Int
world_height = return $ max_pycor_ conf - min_pycor_ conf + 1


{-# WARNING clear_all_plots "TODO" #-}
-- | Clears every plot in the model.
clear_all_plots :: CIO ()
clear_all_plots = do
    (_, a, _, _) <- Reader.ask
    case a of
      ObserverRef _ -> return ()
      _ -> throw $ ContextException "observer" a

{-# WARNING clear_drawing "TODO" #-}
-- | Clears all lines and stamps drawn by turtles. 
clear_drawing :: CIO ()
clear_drawing = do
    (_, a, _, _) <- Reader.ask
    case a of
      ObserverRef _ -> return ()
      _ -> throw $ ContextException "observer" a

{-# INLINE cd #-}
-- | alias for 'clear_drawing'
cd :: CIO ()
cd = clear_drawing

{-# WARNING clear_output "TODO" #-}
-- | Clears all text from the model's output area, if it has one. Otherwise does nothing. 
clear_output :: CIO ()
clear_output = do
    (_, a, _, _) <- Reader.ask
    case a of
      ObserverRef _ -> return ()
      _ -> throw $ ContextException "observer" a


-- | Kills all turtles.
-- Also resets the who numbering, so the next turtle created will be turtle 0.
clear_turtles :: CIO ()
clear_turtles = do
  (tw, a, _, _) <- Reader.ask
  case a of
    ObserverRef _ -> atomic $ lift $ do
                  (MkWorld ps _ ls) <- readTVar tw
                  writeTVar tw (MkWorld ps IM.empty ls)
                  writeTVar __who 0
    _ -> throw $ ContextException "observer" a

{-# INLINE ct #-}
-- | alias for 'clear_turtles'
ct :: CIO ()
ct = clear_turtles

-- | Kills all links.
clear_links :: CSTM ()
clear_links = do
  (tw, s, _,_) <- Reader.ask
  case s of
    ObserverRef _ -> do
                (MkWorld ps ts _) <- lift $ readTVar tw
                lift $ writeTVar tw (MkWorld ps ts M.empty)
    _ -> throw $ ContextException "observer" s

-- | Clears the patches by resetting all patch variables to their default initial values, including setting their color to black. 
clear_patches :: CIO ()
clear_patches = do
  (tw, s, _, _) <- Reader.ask
  case s of
    ObserverRef _ -> atomic $ lift $ do
                  (MkWorld ps _ _) <- readTVar tw
                  T.mapM (\ (MkPatch {pcolor_=tc, plabel_=tl, plabel_color_=tlc, pvars_=to})  -> do
                              writeTVar tc 0
                              writeTVar tl ""
                              writeTVar tlc 9.9
                              mapM_ (`writeTVar` 0) (elems to) -- patches-own to 0
                           ) ps
                  return ()
    _ -> throw $ ContextException "observer" s

{-# INLINE cp #-}
-- | alias for 'clear_patches'
cp :: CIO ()
cp = clear_patches


-- | Clears the tick counter.
-- Does not set the counter to zero. After this command runs, the tick counter has no value. Attempting to access or update it is an error until reset-ticks is called. 
clear_ticks :: CIO ()
clear_ticks = do
    (_, a, _, _) <- Reader.ask
    case a of
      ObserverRef _ -> lift $ writeIORef __tick undefined
      _ -> throw $ ContextException "observer" a

-- | Resets the tick counter to zero, sets up all plots, then updates all plots (so that the initial state of the world is plotted). 
reset_ticks :: CIO ()
reset_ticks = do
    (_, a, _, _) <- Reader.ask
    case a of
      ObserverRef _ -> lift $ writeIORef __tick 0
      _ -> throw $ ContextException "observer" a

-- | Advances the tick counter by one and updates all plots. 
{-# INLINE tick #-}
tick :: CIO ()
tick = tick_advance 1

{-# WARNING tick_advance "TODO: dynamic typing, float" #-}
-- | Advances the tick counter by number. The input may be an integer or a floating point number. (Some models divide ticks more finely than by ones.) The input may not be negative. 
tick_advance :: Double -> CIO ()
tick_advance n = do
  (_, a, _, _) <- Reader.ask
  case a of
    ObserverRef _ -> lift $ modifyIORef' __tick (+n)
    _ -> throw $ ContextException "observer" a

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
foreach :: Monad m => [a] -> (a -> m b) -> m ()
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
repeat_ :: (Monad m) => Int -> m a -> m ()
repeat_ 0 _ = return ()
repeat_ n c = c >> repeat_ (n-1) c

{-# INLINE report #-}
-- | Immediately exits from the current to-report procedure and reports value as the result of that procedure. report and to-report are always used in conjunction with each other. 
-- | NB: IN HLogo, It does not exit the procedure, but it will if the report primitive happens to be the last statement called from the procedure
report :: Monad m => a -> m a
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
n_values :: (Eq a, Monad m, Num a) => a -> (a -> m t) -> m [t]
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
one_of :: [a] -> CSTM [a]
one_of [] = error "empty list"
one_of l = do
  (_,s,_,_) <- Reader.ask
  let ts = case s of
            ObserverRef tg -> tg
            TurtleRef _ t -> tgen t
            PatchRef _ p -> pgen p
            LinkRef _ lg -> lgen lg
            Nobody -> throw DevException
  gen <- lift $ readTVar ts
  let (v,gen') = randomR (0, length l -1) gen
  lift $ writeTVar ts gen'
  return [l !! v]

-- Uses instead agent_one_of when types match
{-# RULES "one_of/AgentRef" one_of = agent_one_of #-}
agent_one_of :: [AgentRef] -> CSTM [AgentRef]
agent_one_of [] = nobody
agent_one_of l = do
  (_,s,_,_) <- Reader.ask
  let ts = case s of
            ObserverRef tg -> tg
            TurtleRef _ t -> tgen t
            PatchRef _ p -> pgen p
            LinkRef _ lg -> lgen lg
            Nobody -> throw DevException
  gen <- lift $ readTVar ts
  let (v,gen') = randomR (0, length l -1) gen
  lift $ writeTVar ts gen'
  return [l !! v]

-- |  From an agentset, reports an agentset of size size randomly chosen from the input set, with no repeats.
-- From a list, reports a list of size size randomly chosen from the input set, with no repeats. 
n_of :: (Eq a) => Int -> [a] -> CSTM [a]
n_of n ls | n == 0     = return []
          | n < 0     = error "negative index"
          | otherwise = do
  [o] <- one_of ls
  ns <- n_of (n-1) (delete o ls)
  return (o:ns)

-- |  From an agentset, reports an agentset of size size randomly chosen from the input set, with no repeats.
-- From a list, reports a list of size size randomly chosen from the input set, with no repeats. 
unsafe_n_of :: (Eq a) => Int -> [a] -> CIO [a]
unsafe_n_of n ls | n == 0     = return []
          | n < 0     = error "negative index"
          | otherwise = do
  [o] <- unsafe_one_of ls
  ns <- unsafe_n_of (n-1) (delete o ls)
  return (o:ns)


-- Uses instead agent_one_of when types match
{-# RULES "n_of/AgentRef" n_of = agent_n_of #-}
agent_n_of :: Int -> [AgentRef] -> CSTM [AgentRef]
agent_n_of n ls | n == 0     = return []
                | n < 0     = error "negative index"
                | otherwise = do
  [o] <- one_of ls
  when (o == Nobody) $ error "empty agentset"
  ns <- n_of (n-1) (delete o ls)
  return (o:ns)


{-# WARNING min_one_of "TODO: currently deterministic and no randomness on tie breaking" #-}
-- | Reports a random agent in the agentset that reports the lowest value for the given reporter. If there is a tie, this command reports one random agent that meets the condition.
min_one_of :: Ord a => [AgentRef] -> CIO a -> CIO [AgentRef]
min_one_of as r = do
  rs <- of_ r as
  return [snd $ minimum $ zip rs as]

{-# WARNING max_one_of "TODO: currently deterministic and no randomness on tie breaking" #-}
-- | Reports the agent in the agentset that has the highest value for the given reporter. If there is a tie this command reports one random agent with the highest value. If you want all such agents, use with-max instead. 
max_one_of :: Ord a => [AgentRef] -> CIO a -> CIO [AgentRef]
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
remove_duplicates :: (Eq a, Monad m) => [a] -> C m [a]
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
shuffle :: Eq a => [a] -> CSTM [a]
shuffle [] = return []
shuffle [x] = return [x]
shuffle l = do 
  [x] <- one_of l
  xs <- shuffle (delete x l)
  return $ x:xs

{-# INLINE sort_ #-}
-- | Reports a sorted list of numbers, strings, or agents. 
sort_ :: (Monad m, Ord a) => [a] -> m [a]
sort_ = return . sort

{-# WARNING sort_by "TODO: requires dynamic_typing" #-}
{-# INLINE sort_by #-}
-- | If the input is a list, reports a new list containing the same items as the input list, in a sorted order defined by the boolean reporter task. 
sort_by :: Monad m => (a -> a -> Ordering) -> [a] -> m [a]
sort_by c l = return $ sortBy c l

-- | Reports a list of agents, sorted according to each agent's value for reporter. Ties are broken randomly. 
sort_on :: Ord a => CSTM a -> [AgentRef] -> CSTM [AgentRef]
sort_on rep as = do
  (tw, s, p, _) <- Reader.ask
  xs <- lift . sequence $ [Reader.runReaderT rep (tw, a, p, s) | a <- as]
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
abs_ :: (Monad m, Num a) => a -> C m a
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
remainder :: Integer -> Integer -> Integer
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
subtract_headings :: (Monad m) => Double -> Double -> C m Double
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
hide_link :: CSTM ()
hide_link = do
  (_, a, _, _) <- Reader.ask
  case a of
    LinkRef _ (MkLink {lhiddenp_ = h}) -> lift $ writeTVar h True
    _ -> throw $ ContextException "link" a

-- | The turtle becomes visible again. 
show_link :: CSTM ()
show_link = do
  (_, a, _, _) <- Reader.ask
  case a of
    LinkRef _ (MkLink {lhiddenp_ = h}) -> lift $ writeTVar h False
    _ -> throw $ ContextException "link" a


-- | Reports the distance between the endpoints of the link. 
link_length :: CSTM Double
link_length = do
  (_, s, _, _) <- Reader.ask
  case s of
    LinkRef (f,t) _ -> do
                [TurtleRef _ (MkTurtle {xcor_ = fx, ycor_ = fy})] <- turtle f
                [TurtleRef _ (MkTurtle {xcor_ = tx, ycor_ = ty})] <- turtle t
                x <- lift $ readTVar fx
                y <- lift $ readTVar fy
                x' <- lift $ readTVar tx
                y' <- lift $ readTVar ty
                return $ sqrt (delta x x' (max_pxcor_ conf) ^ 2 + 
                            delta y y' (max_pycor_ conf) ^ 2)
    _ -> throw $ ContextException "link" s



-- | Report the undirected link between turtle and the caller. If no link exists then it reports nobody. 
link_with :: [AgentRef] -> CSTM [AgentRef]
link_with [TurtleRef x _] = do
  (_, s, _, _) <- Reader.ask
  case s of
    TurtleRef y _ -> do
           lxy <- link x y
           lyx <- link y x
           return $ case (lxy,lyx) of
                      ([Nobody], [Nobody]) -> [Nobody]
                      ([Nobody], _) -> error "directed link"
                      ([LinkRef _ _], [LinkRef _ _]) -> lxy -- return arbitrary 1 of the two link positions
                      (_, [Nobody]) -> error "directed link"
                      _ -> throw DevException
    _ -> throw $ ContextException "turtle" s
link_with a = throw $ TypeException "turtle" (head a)

  
-- | Report the directed link from turtle to the caller. If no link exists then it reports nobody. 
in_link_from :: [AgentRef] -> CSTM [AgentRef]
in_link_from [TurtleRef x _] = do
  (_, s, _, _) <- Reader.ask
  case s of
    TurtleRef y _ -> do
           lxy <- link x y
           lyx <- link y x
           return $ case (lxy,lyx) of
                      ([Nobody], _) -> [Nobody]
                      (_, [Nobody]) -> lxy
                      ([LinkRef _ _], [LinkRef _ _]) -> error "undirected link"
                      _ -> throw DevException
    _ -> throw $ ContextException "turtle" s
in_link_from a = throw $ TypeException "turtle" (head a)



-- | Reports the directed link from the caller to turtle. If no link exists then it reports nobody. 
out_link_to :: [AgentRef] -> CSTM [AgentRef]
out_link_to [TurtleRef x _] = do
  (_, s, _, _) <- Reader.ask
  case s of
    TurtleRef y _ -> do
           lxy <- link x y
           lyx <- link y x
           return $ case (lyx,lxy) of
                      ([Nobody], _) -> [Nobody]
                      (_, [Nobody]) -> lyx
                      ([LinkRef _ _], [LinkRef _ _]) -> error "undirected link"
                      _ -> throw DevException
    _ -> throw $ ContextException "turtle" s
out_link_to a = throw $ TypeException "turtle" (head a)


-- | Reports an agentset of all undirected links connected to the caller. 
my_links :: CSTM [AgentRef]
my_links = do
  (tw,a, _, _) <- Reader.ask 
  case a of
     TurtleRef x _ -> do
             (MkWorld _ _ ls) <- lift $ readTVar tw
             return $ map (uncurry LinkRef) $ M.assocs $ M.intersection (M.filterWithKey (\ (f,_) _ -> f == x) ls) (M.filterWithKey (\ (_,t) _ -> t == x) ls)
     _ -> throw $ ContextException "turtle" a

-- | Reports an agentset of all the directed links going out from the caller to other nodes. 
my_out_links :: CSTM [AgentRef]
my_out_links = do
  (tw, a, _, _) <- Reader.ask 
  case a of
    TurtleRef x _ -> do
                 (MkWorld _ _ ls) <- lift $ readTVar tw
                 return $ map (uncurry LinkRef) $ M.assocs $ M.filterWithKey (\ (f,_) _ -> f == x) ls
    _ -> throw $ ContextException "turtle" a

-- |  Reports an agentset of all the directed links coming in from other nodes to the caller. 
my_in_links :: CSTM [AgentRef]
my_in_links = do
  (tw, a, _, _) <- Reader.ask 
  case a of
    TurtleRef x _ -> do
                (MkWorld _ _ ls) <- lift $ readTVar tw
                return $ map (uncurry LinkRef) $ M.assocs $ M.filterWithKey (\ (_,t) _ -> t == x) ls
    _ -> throw $ ContextException "turtle" a

-- | Reports an empty link agentset. 
no_links :: Monad m => C m [AgentRef]
no_links = return []

-- | Ties end1 and end2 of the link together. If the link is a directed link end1 is the root turtle and end2 is the leaf turtle. The movement of the root turtle affects the location and heading of the leaf turtle. If the link is undirected the tie is reciprocal so both turtles can be considered root turtles and leaf turtles. Movement or change in heading of either turtle affects the location and heading of the other turtle. 
tie :: CSTM ()
tie = do
  (_, a, _, _) <- Reader.ask
  case a of
    LinkRef _ (MkLink {tie_mode = t}) -> lift $ writeTVar t Fixed
    _ -> throw $ ContextException "link" a

-- | Unties end2 from end1 (sets tie-mode to "none") if they were previously tied together. If the link is an undirected link, then it will untie end1 from end2 as well. It does not remove the link between the two turtles. 
untie :: CSTM ()
untie = do
  (_, a, _, _) <- Reader.ask
  case a of
    LinkRef _ (MkLink {tie_mode = t}) -> lift $ writeTVar t None
    _ -> throw $ ContextException "link" a

-- | Reports an agentset containing all of the links anywhere in any of the inputs.
-- | NB: HLogo no support for nested turtle_set concatenation/flattening
link_set :: Monad m => [C m [AgentRef]] -> C m [AgentRef]
link_set ts = liftM (foldr (\ x acc -> if x == Nobody -- filter Nobody
                                      then acc
                                      else case x of -- type check
                                             LinkRef _ _ -> if x `elem` acc -- nub
                                                           then acc
                                                           else x:acc
                                             _ -> throw $ TypeException "link" x
                           ) [] . concat) (sequence ts)

end1 :: CSTM [AgentRef]
end1 = do
  (_,s,_,_) <- Reader.ask
  case s of
    LinkRef (e1, _e2) _ -> turtle e1
    _ -> throw $ ContextException "link" s

end2 :: CSTM [AgentRef]
end2 = do
  (_,s,_,_) <- Reader.ask
  case s of
    LinkRef (_e1, e2) _ -> turtle e2
    _ -> throw $ ContextException "link" s


-- | lifting STM to IO, a wrapper to 'atomically' that optionally (based on a CPP flag) can capture STM statistics 
atomic :: CSTM a -> CIO a
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

{-# WARNING ask "TODO: both splitting" #-}
-- | The specified agent or agentset runs the given commands. 
ask :: CIO a -> [AgentRef] -> CIO ()
ask f as = do
 (tw, s, p,_) <- Reader.ask
 case as of
   [Nobody] -> throw $ TypeException "agentset" Nobody
   _ -> case s of
        Nobody -> throw $ ContextException "agent" Nobody
        ObserverRef _ -> lift (ask' >> ThreadG.wait __tg)
        _ -> lift ask'
    where
     ask' = mapM_ (\ asSection -> 
                       ThreadG.forkIO __tg $ sequence_ [Reader.runReaderT f (tw, a, p, s) | a <- asSection]
                  ) (split numCapabilities as)

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

-- | Internal
split :: Int -> [a] -> [[a]]
split 1 l = [l]
split n l = let (d,m) = length l `divMod` n
                split' 0 _ _ = []
                split' x 0 l' = let (t, rem_list) = splitAt d l'
                               in t : split' (x-1) 0 rem_list
                split' x m' l' = let (t, rem_list) = splitAt (d+1) l'
                                 in t : split' (x-1) (m'-1) rem_list
            in split' n m l

-- | Internal
vsplit :: Int -> [AgentRef] -> [(Int, [AgentRef])]
vsplit n as = IM.toList $ foldl (\ im a -> case a of
                                            PatchRef (x1, _) _ -> IM.insertWith (++) ((x1 + max_x) `div` sector) [a] im
                                            TurtleRef _ (MkTurtle {init_xcor_ = ix}) -> IM.insertWith (++) ((ix + max_x) `div` sector) [a] im
                                            _ -> IM.insertWith (++) n [a] im -- it is not a patch or a turtle, so it is a link, it should be scheduled by GHC and not by us, put it at the extra n
                                ) IM.empty as
              where max_x = max_pxcor_ conf
                    min_x = min_pxcor_ conf
                    sector = (max_x - min_x) `div` n

-- | Internal
hsplit :: Int -> [AgentRef] -> [(Int, [AgentRef])]
hsplit n as = IM.toList $ foldl (\ im a -> case a of
                                            PatchRef (_, y1) _ -> IM.insertWith (++) ((y1 + max_y) `div` sector) [a] im
                                            TurtleRef _ (MkTurtle {init_ycor_ = iy}) -> IM.insertWith (++) ((iy + max_y) `div` sector) [a] im
                                            _ -> IM.insertWith (++) n [a] im -- it is not a patch, so it should be scheduled by GHC and not by us, put it at the extra n
                                ) IM.empty as
              where max_y = max_pycor_ conf
                    min_y = min_pycor_ conf
                    sector = (max_y - min_y) `div` n


-- | For an agent, reports the value of the reporter for that agent (turtle or patch). 
--  For an agentset, reports a list that contains the value of the reporter for each agent in the agentset (in random order). 
of_ :: CIO a -> [AgentRef] -> CIO [a]
of_ f as = do
  (tw, s, p, _) <- Reader.ask
  case as of
    [Nobody] -> throw $ TypeException "agentset" Nobody 
    _ -> case s of
          Nobody -> throw $ ContextException "agent" Nobody
          ObserverRef _ -> lift $ do
             ws <- mapM (\ asi -> liftM snd $ Thread.forkIO (sequence [Reader.runReaderT f (tw, a, p, s) | a <- asi])) (split numCapabilities as)
             rs <- sequence [Thread.result =<< w | w <- ws]
             ThreadG.wait __tg  -- wait for potential internal asks inside the of reporters
             return $ concat rs -- lists traversals can be optimized
          _ -> lift $ do
             ws <- mapM (\ asi -> liftM snd $ Thread.forkIO (sequence [Reader.runReaderT f (tw, a, p, s) | a <- asi])) (split numCapabilities as)
             liftM concat $ sequence [Thread.result =<< w | w <- ws] -- lists traversals can be optimized
                 
  
-- | Takes two inputs: an agentset and a boolean reporter. Reports a new agentset containing only those agents that reported true 
-- in other words, the agents satisfying the given condition. 
with :: CIO Bool -> [AgentRef] -> CIO [AgentRef]
with f as = do
  res <- f `of_` as
  return $ foldr (\ (a, r) l -> if r then a:l else l) [] (zip as res)

{-# WARNING loop  "TODO: use MaybeT or ErrorT" #-}
-- |  Runs the list of commands forever, or until the current procedure exits through use of the stop command or the report command. 
-- NB: Report command will not stop it in HLogo, only the stop primitive. 
-- This command is only run in IO, bcs the command has been implemented
-- using exceptions and exceptions don't work the same in STM. Also
-- it avoids common over-logging that can happen in STM.
loop :: CIO a -> CIO ()
loop c = forever c `catchIO` \ StopException -> return ()

-- | This agent exits immediately from the enclosing to-procedure that was called from 'ask', or ask-like construct (e.g. crt, hatch, sprout). Only the current procedure stops, not all execution for the agent. Also can exit from a top-level (observer) procedure.
stop :: STMorIO m => C m ()
stop = throw StopException

{-# WARNING while  "TODO: use MaybeT or ErrorT" #-}
-- | If reporter reports false, exit the loop. Otherwise run commands and repeat. 
-- This command is only run in IO, bcs the command has been implemented
-- using exceptions and exceptions don't work the same in STM. Also
-- it avoids common over-logging that can happen in STM.
while :: CIO Bool -> CIO a -> CIO ()
while r c = r >>= \ res -> when res $ (c >> while r c) `catchIO` (\ StopException -> return ())

-- Type-safe Casts

is_turtlep :: (Monad m, Typeable a) => a -> C m Bool
is_turtlep a = return $ maybe False (\case [TurtleRef _ _] -> True
                                           _ -> False
                                    )
                                         (cast a :: Maybe [AgentRef])

is_patchp :: (Monad m, Typeable a) => a -> C m Bool
is_patchp a = return $ maybe False (\case [PatchRef _ _] -> True
                                          _ -> False)
                                         (cast a :: Maybe [AgentRef])

is_agentp :: (Monad m, Typeable a) => a -> C m Bool
is_agentp a = return $ maybe False (\case [Nobody] -> False                                       
                                          [_] -> True -- check for a single agent
                                          _ -> False
                                   ) (cast a :: Maybe [AgentRef])

-- alternative but slower implementation is_agentp a = is_turtlep a || is_patchp a


-- | Reports true if value is of the given type, false otherwise. 
is_patch_setp :: (Monad m, Typeable a) => [a] -> C m Bool
is_patch_setp ps = do
  res <- mapM is_patchp ps
  return $ and res


-- | Reports true if value is of the given type, false otherwise. 
is_turtle_setp :: (Monad m, Typeable a) => [a] -> C m Bool
is_turtle_setp ps = do
  res <- mapM is_turtlep ps
  return $ and res

-- | Reports true if value is of the given type, false otherwise.  
is_agentsetp :: (Monad m, Typeable a) => [a] -> C m Bool
is_agentsetp ps = do 
  res <- mapM (\ a -> do
                ip <- is_patchp a
                it <- is_turtlep a 
                il <- is_linkp a
                return $ ip || it || il) ps
  return $ and res

-- Not used, because EDSL, using internal lambda abstractions
-- is_command_taskp
-- is_reporter_taskp

{-# WARNING is_listp "TODO" #-}
--is_listp :: (Monad m, Typeable a) => a -> C m Bool
--is_listp :: (Typeable a, Typeable t) => t -> [a]
--is_listp l =  (cast l :: Typeable a => Maybe [a])
is_listp :: t
is_listp = todo

is_stringp :: (Monad m, Typeable a) => a -> C m Bool
is_stringp s = return $ isJust (cast s :: Maybe String)

is_numberp :: (Monad m, Typeable a) => a -> C m Bool
is_numberp n = return $ is_intp || is_doublep 
               where
                 is_intp = isJust (cast n :: Maybe Int)
                 is_doublep = isJust (cast n :: Maybe Double)

is_linkp :: (Monad m, Typeable a) => a -> C m Bool
is_linkp l = return $ maybe False (\case [LinkRef _ _] -> True
                                         _ -> False)
                                         (cast l :: Maybe [AgentRef])

is_directed_linkp :: (Monad m, Typeable a) => a -> C m Bool
is_directed_linkp l = return $ maybe False (\case [LinkRef _ (MkLink {directed_ = d})] -> d
                                                  _ -> False)
                                         (cast l :: Maybe [AgentRef])

is_undirected_linkp :: (Monad m, Typeable a) => a -> C m Bool
is_undirected_linkp = liftM not . is_directed_linkp



{-# WARNING is_link_setp "TODO: would require a datatype distinction between agentrefs" #-}
-- | Checks only the 1st element
is_link_setp :: (Monad m, Typeable a) => [a] -> C m Bool
is_link_setp (l:_) = is_linkp l
is_link_setp _ = throw DevException

-- | This turtle creates number new turtles. Each new turtle inherits of all its variables, including its location, from its parent. (Exceptions: each new turtle will have a new who number)
hatch :: Int -> CSTM [AgentRef]
hatch n = do
  (tw, a, _, _) <- Reader.ask
  case a of
#ifdef STATS_STM
    TurtleRef _ (MkTurtle _w bd c h x y s l lc hp sz ps pm tarr _ _ix _iy _tt _ts) -> do
#else
    TurtleRef _ (MkTurtle _w bd c h x y s l lc hp sz ps pm tarr _ _ix _iy) -> do
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
            let addTurtles ts' (MkWorld ps_ ts_ ls_)  = MkWorld ps_ (ts_ `IM.union` ts') ls_
            lift $ modifyTVar' tw (addTurtles ns) 
            return $ map (uncurry TurtleRef) $ IM.toList ns -- todo: can be optimized

    _ -> throw $ ContextException "turtle" a

-- | The turtle sets its x and y coordinates to be the same as the given agent's.
-- (If that agent is a patch, the effect is to move the turtle to the center of that patch.) 
move_to :: [AgentRef] -> CSTM ()
move_to a = do
  (_,s,_,_) <- Reader.ask
  case s of
    TurtleRef _ (MkTurtle {xcor_ = tx, ycor_ = ty}) -> 
              case a of
                [PatchRef (px, py) _] -> lift $ writeTVar tx (fromIntegral px) >> writeTVar ty (fromIntegral py)
                [TurtleRef _ (MkTurtle {xcor_ = tx', ycor_ = ty'})] -> lift $ do
                                                                      x' <- readTVar tx'
                                                                      y' <- readTVar ty'
                                                                      writeTVar tx x'
                                                                      writeTVar ty y'
                _ -> throw $ TypeException "turtle or patch" (head a)
    _ -> throw $ ContextException "turtle" s

unsafe_random_xcor :: CIO Double
unsafe_random_xcor = lift $ getStdRandom $ randomR (fromIntegral $ min_pxcor_ conf, fromIntegral $ max_pxcor_ conf)

unsafe_random_ycor :: CIO Double
unsafe_random_ycor = lift $ getStdRandom $ randomR (fromIntegral $ min_pycor_ conf, fromIntegral $ max_pycor_ conf)

unsafe_random_pxcor :: CIO Int
unsafe_random_pxcor = lift $ getStdRandom $ randomR (min_pxcor_ conf, max_pxcor_ conf)

unsafe_random_pycor :: CIO Int
unsafe_random_pycor = lift $ getStdRandom $ randomR (min_pycor_ conf, max_pycor_ conf)

unsafe_random               :: (Random a , Eq a, Ord a, Num a) => a -> CIO a
unsafe_random x | x == 0 = return 0
                | x < 0 = lift $ getStdRandom $ randomR (x,0)
                | x > 0 = lift $ getStdRandom $ randomR (0,x)
                | otherwise = throw DevException

unsafe_random_float :: Double -> CIO Double
unsafe_random_float x | x == 0 = return 0
                      | x < 0 = lift $ getStdRandom $ randomR (x,0)
                      | x > 0 = lift $ getStdRandom $ randomR (0,x)
                      | otherwise = throw DevException


-- | Sets the seed of the pseudo-random number generator to the integer part of number.
unsafe_random_seed :: Int -> IO ()
unsafe_random_seed n = setStdGen $ mkStdGen n


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

-- | Reports an agentset containing all the turtles that are on the given patch or patches, or standing on the same patch as the given turtle or turtles. 
turtles_on :: [AgentRef] -> CIO [AgentRef]
turtles_on [] = return []
turtles_on ps@(PatchRef _ _ : _) = with (liftM (flip elem ps . head) patch_here) =<< turtles
turtles_on ts@(TurtleRef _ _ : _) = turtles_on =<< of_ (liftM head patch_here) ts
turtles_on (a:_) = throw $ ContextException "turtle or patch agentset" a


{-# WARNING at_points "TODO" #-}
-- |  Reports a subset of the given agentset that includes only the agents on the patches the given distances away from this agent. The distances are specified as a list of two-item lists, where the two items are the x and y offsets.
-- If the caller is the observer, then the points are measured relative to the origin, in other words, the points are taken as absolute patch coordinates.
-- If the caller is a turtle, the points are measured relative to the turtle's exact location, and not from the center of the patch under the turtle. 
at_points :: [AgentRef] -> [(Double, Double)] -> CSTM [AgentRef]
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
every :: Double -> CIO a -> CIO ()
every n a = a >> wait n

-- | Wait the given number of seconds. (This needn't be an integer; you can specify fractions of seconds.) Note that you can't expect complete precision; the agent will never wait less than the given amount, but might wait slightly more. 
-- | NB: Works differently than NetLogo, in that only the calling thread is suspended, not the whole simulation
wait :: (RealFrac r) => r -> CIO ()
wait n = lift $ threadDelay (round $ n * 1000000)

unsafe_one_of :: [a] -> CIO [a]
unsafe_one_of [] = error "empty list"
unsafe_one_of l = do
  v <- lift $ randomRIO (0, length l -1)
  return [l !! v]

-- Uses instead agent_one_of when types match
{-# RULES "unsafe_one_of/AgentRef" unsafe_one_of = agent_unsafe_one_of #-}

agent_unsafe_one_of :: [AgentRef] -> CIO [AgentRef]
agent_unsafe_one_of [] = nobody
agent_unsafe_one_of l = do
  v <- lift $ randomRIO (0, length l -1)
  return [l !! v]

-- | Reports a new list containing the same items as the input list, but in randomized order. 
unsafe_shuffle :: Eq a => [a] -> CIO [a]
unsafe_shuffle [] = return []
unsafe_shuffle [x] = return [x]
unsafe_shuffle l = do 
  [x] <- unsafe_one_of l
  xs <- unsafe_shuffle (delete x l)
  return $ x:xs

{-# DEPRECATED unsafe_show "it is slightly faster than show since it does not involve any STM, but it should not matter that much and also printing is discourage on real benchmarking." #-} 
-- | Considered unsafe; the output may be mangled, because of many threads writing to the same output
unsafe_show :: Show a => a -> CIO ()
unsafe_show a = do
  (_, r, _, _) <- Reader.ask
  lift $ putStrLn $ (case r of
                           ObserverRef _ -> "observer: "
                           PatchRef (x,y) _ -> "(patch " ++ Prelude.show x ++ " " ++ Prelude.show y ++ "): "
                           LinkRef (x,y) _ -> "(link " ++ Prelude.show x ++ " " ++ Prelude.show y ++ "): "
                           TurtleRef i _ -> "(turtle " ++ Prelude.show i ++ "): "
                           Nobody -> throw DevException
                    )   ++ Prelude.show a

{-# DEPRECATED unsafe_print "it is slightly faster than print since it does not involve any STM, but it should not matter that much and also printing is discourage on real benchmarking." #-} 
-- | Considered unsafe; the output may be mangled, because of many threads writing to the same output
unsafe_print :: Show a => a -> CIO ()
unsafe_print a = lift $ Prelude.print a

stats_stm :: CIO Double
stats_stm = 
#ifndef STATS_STM 
   error "library not compiled with stats-stm flag enabled"
#else
   do
    (tw, _, _, _) <- Reader.ask
    MkWorld wps wts wls <- lift $ readTVarIO tw
    (vtt, vts) <- lift $ F.foldlM (\ (acct, accs) (MkTurtle {ttotalstm=tt, tsuccstm=ts}) -> do
                                  rtt <- readIORef tt
                                  rts <- readIORef ts
                                  return (acct+rtt, accs+rts)) (0,0) wts
    (vpt, vps) <- lift $ F.foldlM (\ (acct, accs) (MkPatch {ptotalstm=pt, psuccstm=ps}) -> do
                                  rpt <- readIORef pt
                                  rps <- readIORef ps
                                  return (acct+rpt, accs+rps)) (0,0) wps
    (vlt, vls) <- lift $ F.foldlM (\ (acct, accs) (MkLink {ltotalstm=pt, lsuccstm=ps}) -> do
                                  rlt <- readIORef pt
                                  rls <- readIORef ps
                                  return (acct+rlt, accs+rls)) (0,0) wls
    let total = fromIntegral $ vtt+vpt+vlt
    let suc = fromIntegral $ vts+vps+vls
    return $ (total - suc) / total
#endif


with_breed :: (String -> String) -> CSTM ()
with_breed f = do
  (_,s,_,_) <- Reader.ask
  case s of
    TurtleRef _ (MkTurtle {breed_ = tb}) -> lift $ modifyTVar' tb f
    --LinkRef _ (MkLink {lbreed_ = tb}) -> lift $ modifyTVar' tb f
    _ -> throw $ ContextException "turtle" s

with_color :: (Double -> Double) -> CSTM ()
with_color f = do
  (_,s,_,_) <- Reader.ask
  case s of
    TurtleRef _ (MkTurtle {color_ = tb}) -> lift $ modifyTVar' tb f
    LinkRef _ (MkLink {lcolor_ = tb}) -> lift $ modifyTVar' tb f
    _ -> throw $ ContextException "turtle or link" s

with_heading :: (Double -> Double) -> CSTM ()
with_heading f = do
  (_,s,_,_) <- Reader.ask
  case s of
    TurtleRef _ (MkTurtle {heading_ = tb}) -> lift $ modifyTVar' tb f
    _ -> throw $ ContextException "turtle" s

with_shape :: (String -> String) -> CSTM ()
with_shape f = do
  (_,s,_,_) <- Reader.ask
  case s of
    TurtleRef _ (MkTurtle {shape_ = tb}) -> lift $ modifyTVar' tb f
    _ -> throw $ ContextException "turtle" s

with_label :: (String -> String) -> CSTM ()
with_label f = do
  (_,s,_,_) <- Reader.ask
  case s of
    TurtleRef _ (MkTurtle {label_ = tb}) -> lift $ modifyTVar' tb f
    LinkRef _ (MkLink {llabel_ = tb}) -> lift $ modifyTVar' tb f
    _ -> throw $ ContextException "turtle or link" s

with_label_color :: (Double -> Double) -> CSTM ()
with_label_color f = do
  (_,s,_,_) <- Reader.ask
  case s of
    TurtleRef _ (MkTurtle {label_color_ = tb}) -> lift $ modifyTVar' tb f
    LinkRef _ (MkLink {llabel_color_ = tb}) -> lift $ modifyTVar' tb f
    _ -> throw $ ContextException "turtle or link" s

with_size :: (Double -> Double) -> CSTM ()
with_size f = do
  (_,s,_,_) <- Reader.ask
  case s of
    TurtleRef _ (MkTurtle {size_ = tb}) -> lift $ modifyTVar' tb f
    _ -> throw $ ContextException "turtle" s

with_pcolor :: (Double -> Double) -> CSTM ()
with_pcolor f = do
  (_,s,_,_) <- Reader.ask
  case s of
    PatchRef _ (MkPatch {pcolor_ = tb}) -> lift $ modifyTVar' tb f
    _ -> throw $ ContextException "patch" s

with_plabel :: (String -> String) -> CSTM ()
with_plabel f = do
  (_,s,_,_) <- Reader.ask
  case s of
    PatchRef _ (MkPatch {plabel_ = tb}) -> lift $ modifyTVar' tb f
    _ -> throw $ ContextException "patch" s

with_plabel_color :: (Double -> Double) -> CSTM ()
with_plabel_color f = do
  (_,s,_,_) <- Reader.ask
  case s of
    PatchRef _ (MkPatch {plabel_color_ = tb}) -> lift $ modifyTVar' tb f
    _ -> throw $ ContextException "patch" s

snapshot :: CIO ()
snapshot = do
  ( _, s, _,  _) <- Reader.ask
  case s of
    ObserverRef _ -> do
             ticksNow <- ticks
             ps <- patch_size
             max_x <- max_pxcor
             min_x <- min_pycor
             let sizeSpec = Diag.mkWidth (fromIntegral (ps * (max_x + abs min_x + 1)))
             let output = "snapshot" ++ Prelude.show (round ticksNow :: Int) ++ ".eps"
             prs <- patches
             diagPatches <- lift $ mapM (\ (PatchRef (px,py) p) -> do 
                                   c <- readTVarIO $ pcolor_ p
                                   t <- readTVarIO $ plabel_ p
                                   let [r,g,b] = extract_rgb c
                                   return (Diag.p2 (fromIntegral px, fromIntegral py), Diag.text t Diag.# Diag.fc (sRGB24 255 255 255) Diag.<> Diag.square 1 Diag.# Diag.fc (sRGB24 r g b) :: Diag.Diagram Postscript)
                                ) prs
             trs <- turtles
             diagTurtles <- lift $ mapM (\ (TurtleRef _ t) -> do 
                                          x <- readTVarIO $ xcor_ t
                                          y <- readTVarIO $ ycor_ t
                                          c <- readTVarIO $ color_ t
                                          h <- readTVarIO $ heading_ t
                                          ts <- readTVarIO $ size_ t
                                          let [r,g,b] = extract_rgb c
                                          return (Diag.p2 (x, y), Diag.eqTriangle ts Diag.# Diag.fc (sRGB24 r g b) Diag.# Diag.scaleX 0.5 Diag.# Diag.rotate (-h Diag.@@ Diag.deg) :: Diag.Diagram Postscript)
                                ) trs

             
             lift $ Diag.renderDia Postscript (PostscriptOptions output sizeSpec EPS) (Diag.position diagTurtles `Diag.atop` Diag.position diagPatches)
    _ -> throw $ ContextException "observer" s
  

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

-- | A class to take advantage of faster 'readTVarIO'. Any commands that do not do STM side-effects (IO effects allowed)
--  or only depend on 'readTVar', belong here. The correct lifting (STM or IO) is left to type inference.
class (Monad m) => STMorIO m where
    -- |  Reports an agentset containing all the turtles on the caller's patch (including the caller itself if it's a turtle). 
    turtles_here :: C m [AgentRef]
    -- |  Reports an agentset containing the turtles on the patch (dx, dy) from the caller. (The result may include the caller itself if the caller is a turtle.) 
    turtles_at :: Double -> Double -> C m [AgentRef] -- ^ dx -> dy -> CSTM (Set AgentRef)
    -- | patch-here reports the patch under the turtle. 
    patch_here :: C m [AgentRef]
    -- | Reports the agentset consisting of all patches. 
    patches :: C m [AgentRef]
    -- | Given the x and y coordinates of a point, reports the patch containing that point. 
    patch :: Double -> Double -> C m [AgentRef]
    -- | Reports the agentset consisting of all turtles. 
    turtles :: C m [AgentRef]
    -- | Reports the turtle with the given who number, or nobody if there is no such turtle. For breeded turtles you may also use the single breed form to refer to them. 
    turtle :: Int -> C m [AgentRef]
    -- | This is a built-in turtle variable. It indicates the direction the turtle is facing. 
    heading :: C m Double
    -- | This is a built-in turtle variable. It holds the current x coordinate of the turtle. 
    xcor :: C m Double
    pcolor :: C m Double
    -- | This is a built-in turtle variable. It holds the current y coordinate of the turtle.
    ycor :: C m Double
    -- | This is a built-in turtle variable. It holds the turtle's "who number" or ID number, an integer greater than or equal to zero. You cannot set this variable; a turtle's who number never changes. 
    color :: C m Double
    breed :: C m String
    -- | Reports the distance from this agent to the given turtle or patch. 
    distance :: [AgentRef] -> C m Double
    -- | Reports the distance from this agent to the point (xcor, ycor). 
    distancexy :: Double -> Double -> C m Double
    -- | Reports the heading from this agent to the given agent. 
    towards :: [AgentRef] -> C m Double
    -- | Reports an agentset that includes only those agents from the original agentset whose distance from the caller is less than or equal to number. (This can include the agent itself.) 
    in_radius :: [AgentRef] -> Double -> C m [AgentRef]
    -- | Given the who numbers of the endpoints, reports the link connecting the turtles. If there is no such link reports nobody. To refer to breeded links you must use the singular breed form with the endpoints. 
    -- | Reports an agentset containing the 8 surrounding patches
    neighbors :: C m [AgentRef]
    -- | Reports an agentset containing the 4 surrounding patches
    neighbors4 :: C m [AgentRef]
    link :: Int -> Int -> C m [AgentRef]
    -- | Reports the agentset consisting of all links. 
    links :: C m [AgentRef]
    readGlobal :: TVar Double -> C m Double
    readTurtle :: Int -> C m Double
    readPatch :: Int -> C m Double
    readLink :: Int -> C m Double
    timer :: C m Double
    reset_timer :: C m ()
    -- | Prints value in the Command Center, preceded by this agent, and followed by a carriage return.
    --
    -- HLogo-specific: There are no guarantees on which agent will be prioritized to write on the stdout. The only guarantee is that in case of show inside an 'atomic' transaction, no 'show' will be repeated if the transaction is retried. Compared to 'unsafe_show', the output is not mangled.
    show :: Show a => a -> C m ()
    -- | Prints value in the Command Center, followed by a carriage return. 
    --
    -- HLogo-specific: There are no guarantees on which agent will be prioritized to write on the stdout. The only guarantee is that in case of print inside an 'atomic' transaction, no 'print' will be repeated if the transaction is retried. Compared to 'unsafe_print', the output is not mangled.
    print :: Show a => a -> C m ()
    -- | Reports the current value of the tick counter. The result is always a number and never negative. 
    ticks :: C m Double


{-# WARNING towards "TODO: wrapping" #-}
{-# WARNING timer "safe, but some might considered it unsafe with respect to STM, since it may poll the clock multiple times. The IO version of it is totally safe" #-}
{-# WARNING reset_timer "safe, but some might considered it unsafe with respect to STM, since it may poll the clock multiple times. The IO version of it is totally safe" #-}
{-# WARNING ticks "TODO: dynamic typing, integer or float" #-}

instance STMorIO STM where
  turtles_here = do
    [s] <- self
    [PatchRef (px,py) _] <- case s of
                             TurtleRef _ _ -> patch_here
                             p -> return [p]
    ts <- turtles
    filterM (\ (TurtleRef _ (MkTurtle {xcor_ = x, ycor_ = y})) -> do 
               x' <- lift $ readTVar x
               y' <- lift $ readTVar y
               return $ round x' == px && round y' == py
            ) ts
  turtles_at x y = do
    [PatchRef (px, py) _] <- patch_at x y
    ts <- turtles
    filterM (\ (TurtleRef _ (MkTurtle {xcor_ = tx, ycor_ = ty})) -> do 
               x' <- lift $ readTVar tx
               y' <- lift $ readTVar ty
               return $ round x' == px && round y' == py
            ) ts
  patch_here = do
    (_, a, _, _) <- Reader.ask
    case a of
      TurtleRef _ (MkTurtle {xcor_ = x, ycor_ = y}) -> do
                 x' <- lift $ readTVar x
                 y' <- lift $ readTVar y
                 patch x' y'
      _ -> throw $ ContextException "patch" a
  patches = do
    (tw,_, _, _) <- Reader.ask
    (MkWorld ps _ _) <- lift $ readTVar tw
    return $ M.foldrWithKey (\ k x ks -> PatchRef k x: ks) [] ps
  patch x y = do
    (tw,_, _, _) <- Reader.ask
    (MkWorld ps _ _) <- lift $ readTVar tw
    return $ if (not (horizontal_wrap_ conf) && (x' > max_pxcor_ conf || x' < min_pxcor_ conf)) || (not (vertical_wrap_ conf) && (y' > max_pycor_ conf || y' < min_pycor_ conf))
             then [Nobody]
             else
                 [PatchRef (x'', y'') (ps M.! (x'', y''))]
           where
             x' = round x
             y' = round y
             (x'',y'') = (      -- normalize
                          ((x' + max_pxcor_ conf) `mod` (max_pxcor_ conf*2+1)) - max_pxcor_ conf,
                          ((y' + max_pycor_ conf) `mod` (max_pycor_ conf*2+1)) - max_pycor_ conf
                         )

  turtles = do
    (tw,_, _, _) <- Reader.ask
    MkWorld _ ts _ <- lift $ readTVar tw
    return $ IM.foldrWithKey (\ k x ks -> TurtleRef k x: ks) [] ts

  turtle n = do
    (tw,_, _, _) <- Reader.ask
    (MkWorld _ ts _) <- lift $ readTVar tw
    return $ maybe [Nobody] (return . TurtleRef n) $ IM.lookup n ts

  heading = do
    (_,a, _, _) <- Reader.ask
    case a of
      TurtleRef _ (MkTurtle {heading_ = h}) -> lift $ readTVar h
      _ -> throw (ContextException "turtle" a)

  xcor = do
    (_,a, _, _) <- Reader.ask
    case a of
      TurtleRef _ (MkTurtle {xcor_ = x}) -> lift $ readTVar x
      _ -> throw $ ContextException "turtle" a

  pcolor = do
    (_,a, _, _) <- Reader.ask
    case a of
      PatchRef _ (MkPatch {pcolor_ = tc}) -> lift $ readTVar tc
      TurtleRef _ _ -> do
             [PatchRef _ (MkPatch {pcolor_ = tc})] <- patch_here
             lift $ readTVar tc
      _ -> throw $ ContextException "turtle or patch" a
  ycor = do
    (_,a, _, _) <- Reader.ask
    case a of
      TurtleRef _ (MkTurtle {ycor_ = y}) -> lift $ readTVar y
      _ -> throw $ ContextException "turtle" a

  color = do
    (_,a, _, _) <- Reader.ask
    case a of
      TurtleRef _ (MkTurtle {color_ = c}) -> lift $ readTVar c
      LinkRef _ (MkLink {lcolor_ = c}) -> lift $ readTVar c
      _ -> throw $ ContextException "turtle or link" a

  breed = do
    (_,a, _, _) <- Reader.ask
    case a of
      TurtleRef _ (MkTurtle {breed_ = b}) -> lift $ readTVar b
      LinkRef _ (MkLink {lbreed_ = b}) -> return b
      _ -> throw $ ContextException "turtle or link" a

  distance [PatchRef (x,y) _] = distancexy (fromIntegral x) (fromIntegral y)
  distance [TurtleRef _ (MkTurtle {xcor_ = tx, ycor_ = ty})] = do
    x <- lift $ readTVar tx
    y <- lift $ readTVar ty
    distancexy x y
  distance _ = throw $ TypeException "single turtle or patch" Nobody

  distancexy x' y' = do
    (_,a,_,_) <- Reader.ask
    (x,y) <- case a of
              PatchRef (x,y) _ -> return (fromIntegral x, fromIntegral y)
              TurtleRef _ (MkTurtle {xcor_ = tx, ycor_ = ty}) -> liftM2 (,) (lift $ readTVar tx) (lift $ readTVar ty)
              _ -> throw $ ContextException "turtle or patch" a
    return $ sqrt (deltaX x x' ^ 2 + 
                deltaY y y' ^ 2)
    where
      deltaX a1 a2 = if horizontal_wrap_ conf
                     then min 
                              (abs (a2 - a1))
                              (abs (a2 - (fromIntegral $ max_pxcor_ conf) - a1 + (fromIntegral $ min_pxcor_ conf) + 1))
                     else abs (a2 -a1)
      deltaY a1 a2 = if vertical_wrap_ conf
                     then min 
                              (abs (a2 - a1)) 
                              (abs (a2 - (fromIntegral $ max_pycor_ conf) - a1 + (fromIntegral $ min_pycor_ conf) + 1))
                     else abs (a2 -a1)

  towards a = do
    ( _, s, _,  _) <- Reader.ask
    (x1,y1) <- case s of
              PatchRef (x,y) _ -> return (fromIntegral x, fromIntegral y)
              TurtleRef _ (MkTurtle {xcor_ = tx, ycor_ = ty}) -> do
                     x <- lift $ readTVar tx
                     y <- lift $ readTVar ty
                     return (x,y)
              _ -> throw $ ContextException "turtle or patch" s
    (x2,y2) <- case a of
                [PatchRef (x,y) _] -> return (fromIntegral x, fromIntegral y)
                [TurtleRef _ (MkTurtle {xcor_ = tx, ycor_ = ty})] -> do
                     x <- lift $ readTVar tx
                     y <- lift $ readTVar ty
                     return (x,y)
                _ -> throw $ ContextException "turtle or patch" (head a)
    let dx' = x2 - x1
    let dy' = y2 - y1
    return $ if dx' == 0
              then
                  if dy' > 0 
                  then 0 
                  else 180
              else
                  if dy' == 0
                  then if dx' > 0 
                       then 90 
                       else 270
                  else (270 + toDegrees (pi + atan2 (-dy') dx')) `mod_` 360

  in_radius as n = do
    ( _, a, _, _) <- Reader.ask
    (x, y) <- case a of
               PatchRef (x,y) _ -> return (fromIntegral x, fromIntegral y)
               TurtleRef _ (MkTurtle {xcor_ = tx, ycor_ = ty}) -> liftM2 (,) (lift $ readTVar tx) (lift $ readTVar ty)
               _ -> throw $ ContextException "turtle or patch" a
    filterM (\ (TurtleRef _ (MkTurtle {xcor_ = tx', ycor_ = ty'})) -> do 
               x' <- lift $ readTVar tx'
               y' <- lift $ readTVar ty'
               return $ sqrt (delta x x' (fromIntegral (max_pxcor_ conf) :: Int) ^ (2::Int) + 
                           delta y y' (fromIntegral (max_pycor_ conf) :: Int) ^ (2::Int)) <= n) as

  neighbors = do
    ( _, a, _,  _) <- Reader.ask
    (x,y) <- case a of
      PatchRef (x,y) _ ->  return (fromIntegral x, fromIntegral y)
      TurtleRef _ (MkTurtle {xcor_ = tx, ycor_ = ty}) -> liftM2 (,) (lift $ readTVar tx) (lift $ readTVar ty)
      _ -> throw $ ContextException "turtle or patch" a
    patch_set [p (x-1) (y-1),
               p (x-1) y,
               p (x-1) (y+1),
               p x (y-1),
               p x (y+1),
               p (x+1) (y-1),
               p (x+1) y,
               p (x+1) (y+1)
              ]
    where p x y = if (not (horizontal_wrap_ conf) && (x > fromIntegral (max_pxcor_ conf) || x < fromIntegral (min_pxcor_ conf))) || (not (vertical_wrap_ conf) && (y > fromIntegral (max_pycor_ conf) || y < fromIntegral (min_pycor_ conf)))
                  then return []
                  else patch x y

  neighbors4 = do
    ( _, a, _,  _) <- Reader.ask
    (x,y) <- case a of
      PatchRef (x,y) _ ->  return (fromIntegral x, fromIntegral y)
      TurtleRef _ (MkTurtle {xcor_ = tx, ycor_ = ty}) -> liftM2 (,) (lift $ readTVar tx) (lift $ readTVar ty)
      _ -> throw $ ContextException "turtle or patch" a
    patch_set [p (x-1) y,
               p (x+1) y,
               p x (y-1),
               p x (y+1)
              ]
    where p x y = if (not (horizontal_wrap_ conf) && (x > fromIntegral (max_pxcor_ conf) || x < fromIntegral (min_pxcor_ conf))) || (not (vertical_wrap_ conf) && (y > fromIntegral (max_pycor_ conf) || y < fromIntegral (min_pycor_ conf)))
                  then return []
                  else patch x y

  link f t = do
    (tw,_, _,_) <- Reader.ask
    (MkWorld _ _ ls) <- lift $ readTVar tw
    return [maybe Nobody (LinkRef (f,t)) $ M.lookup (f,t) ls]


  links = do
    (tw,_, _,_) <- Reader.ask
    (MkWorld _ _ ls) <- lift $ readTVar tw
    return $ nubBy checkForUndirected $ M.foldrWithKey (\ k x ks -> LinkRef k x: ks) [] ls
        where
          checkForUndirected (LinkRef (e1,e2) (MkLink {directed_ = False})) (LinkRef (e1',e2') (MkLink {directed_ = False})) = e1 == e2' && e1' == e2
          checkForUndirected _ _ = False

  readGlobal = lift . readTVar

  readTurtle i = do
    (_,a ,_,_) <- Reader.ask
    case a of
      TurtleRef _ (MkTurtle {tvars_ = pv}) -> lift $ readTVar (pv ! i)
      _ -> throw $ ContextException "turtle" a

  readPatch i = do 
    (_,a,_,_) <- Reader.ask
    case a of
      PatchRef _ (MkPatch {pvars_ = pv}) -> lift $ readTVar $ pv ! i
      TurtleRef _ _ -> patch_here >>= \ ([PatchRef _ (MkPatch {pvars_ = pv})]) -> lift $ readTVar $ pv !i
      _ -> throw $ ContextException "turtle or patch" a

  readLink i = do
    (_,a ,_,_) <- Reader.ask
    case a of
      LinkRef _ (MkLink {lvars_ = pv}) -> lift $ readTVar (pv ! i)
      _ -> throw $ ContextException "link" a

  timer = lift $ do
      t <- readTVar __timer
      t' <- unsafeIOToSTM getCurrentTime
      return $ realToFrac (t' `diffUTCTime` t)              

  reset_timer = do
      t <- lift $ unsafeIOToSTM getCurrentTime
      lift $ writeTVar __timer t
  show a = do
      (_, r, p, _) <- Reader.ask
      lift $ writeTQueue p $ (case r of
                               ObserverRef _ -> "observer: "
                               PatchRef (x,y) _ -> "(patch " ++ Prelude.show x ++ " " ++ Prelude.show y ++ "): "
                               LinkRef (x,y) _ -> "(link " ++ Prelude.show x ++ " " ++ Prelude.show y ++ "): "
                               TurtleRef i _ -> "(turtle " ++ Prelude.show i ++ "): "
                               Nobody -> throw DevException
                            )   ++ Prelude.show a
  print a = do
      (_, _, p, _) <- Reader.ask
      lift $ writeTQueue p $ Prelude.show a

  ticks = lift $ unsafeIOToSTM $ readIORef __tick



instance STMorIO IO where
  turtles_here = do
    (_,s,_,_) <- Reader.ask
    h <- case s of
          TurtleRef _ _ -> patch_here
          PatchRef _ _ -> return [s]
          _ -> throw $ ContextException "turtle or patch" s
    ts <- turtles
    with (return . (== h) =<< patch_here) ts
  turtles_at x y = do
    p <- patch_at x y
    with (return . (== p)  =<< patch_here) =<< turtles
  patch_here = do
    (_,s, _, _) <- Reader.ask
    case s of
      TurtleRef _ (MkTurtle {xcor_ = x, ycor_ = y}) -> do
                        x' <- lift $ readTVarIO x
                        y' <- lift $ readTVarIO y
                        patch x' y'
      _ -> throw $ ContextException "turtle" s

  patches = do
    (tw,_, _, _) <- Reader.ask
    (MkWorld ps _ _) <- lift $ readTVarIO tw
    return $ M.foldrWithKey (\ k x ks -> PatchRef k x: ks) [] ps

  patch x y = do
    (tw,_, _, _) <- Reader.ask
    (MkWorld ps _ _) <- lift $ readTVarIO tw
    return $ if (not (horizontal_wrap_ conf) && (x' > max_pxcor_ conf || x' < min_pxcor_ conf)) || (not (vertical_wrap_ conf) && (y' > max_pycor_ conf || y' < min_pycor_ conf))
             then [Nobody]
             else
                 [PatchRef (x'', y'') (ps M.! (x'', y''))]
           where
             x' = round x
             y' = round y
             (x'',y'') = (      -- normalize
                          ((x' + max_pxcor_ conf) `mod` (max_pxcor_ conf*2+1)) - max_pxcor_ conf,
                          ((y' + max_pycor_ conf) `mod` (max_pycor_ conf*2+1)) - max_pycor_ conf
                         )

  turtles = do
    (tw,_, _, _) <- Reader.ask
    (MkWorld _ ts _) <- lift $ readTVarIO tw
    return $ IM.foldrWithKey (\ k x ks -> TurtleRef k x: ks) [] ts

  turtle n = do
    (tw,_, _, _) <- Reader.ask
    (MkWorld _ ts _) <- lift $ readTVarIO tw
    return [TurtleRef n (ts IM.! n)]

  heading = do
    (_,a, _, _) <- Reader.ask
    case a of
      TurtleRef _ (MkTurtle {heading_ = h}) -> lift $ readTVarIO h
      _ -> throw (ContextException "turtle" a)

  xcor = do
    (_,a, _, _) <- Reader.ask
    case a of
      TurtleRef _ (MkTurtle {xcor_ = x}) -> lift $ readTVarIO x
      _ -> throw $ ContextException "turtle" a

  pcolor = do
    (_,a, _, _) <- Reader.ask
    case a of
      PatchRef _ (MkPatch {pcolor_ = tc}) -> lift $ readTVarIO tc
      TurtleRef _ _ -> do
             [PatchRef _ (MkPatch {pcolor_ = tc})] <- patch_here
             lift $ readTVarIO tc
      _ -> throw $ ContextException "patch" a

  ycor = do
    (_,a, _, _) <- Reader.ask
    case a of
      TurtleRef _ (MkTurtle {ycor_ = y}) -> lift $ readTVarIO y
      _ -> throw $ ContextException "turtle" a

  color = do
    (_,a, _, _) <- Reader.ask
    case a of
      TurtleRef _ (MkTurtle {color_ = c}) -> lift $ readTVarIO c
      _ -> throw $ ContextException "turtle" a

  breed = do
    (_,a, _, _) <- Reader.ask
    case a of
      TurtleRef _ (MkTurtle {breed_ = b}) -> lift $ readTVarIO b
      LinkRef _ (MkLink {lbreed_ = b}) -> return b
      _ -> throw $ ContextException "turtle or link" a

  distance [PatchRef (x,y) _] = distancexy (fromIntegral x) (fromIntegral y)
  distance [TurtleRef _ (MkTurtle {xcor_ = tx, ycor_ = ty})] = do
    x <- lift $ readTVarIO tx
    y <- lift $ readTVarIO ty
    distancexy x y
  distance _ = throw $ ContextException "single turtle or patch" Nobody


  distancexy x' y' = do
    (_,a,_,_) <- Reader.ask
    (x,y) <- case a of
              PatchRef (x,y) _ -> return (fromIntegral x, fromIntegral y)
              TurtleRef _ (MkTurtle {xcor_ = tx, ycor_ = ty}) -> liftM2 (,) (lift $ readTVarIO tx) (lift $ readTVarIO ty)
              _ -> throw $ ContextException "turtle or patch" a
    return $ sqrt (deltaX x x' ^ 2 + 
                deltaY y y' ^ 2)
    where
      deltaX a1 a2 = if horizontal_wrap_ conf
                     then min 
                              (abs (a2 - a1))
                              (abs (a2 - fromIntegral (max_pxcor_ conf) - a1 + fromIntegral (min_pxcor_ conf) + 1))
                     else abs (a2 -a1)
      deltaY a1 a2 = if vertical_wrap_ conf
                     then min 
                              (abs (a2 - a1)) 
                              (abs (a2 - fromIntegral (max_pycor_ conf) - a1 + fromIntegral (min_pycor_ conf) + 1))
                     else abs (a2 -a1)

  towards a = do
    (_, s, _,  _) <- Reader.ask
    (x1,y1) <- case s of
              PatchRef (x,y) _ -> return (fromIntegral x, fromIntegral y)
              TurtleRef _ (MkTurtle {xcor_ = tx, ycor_ = ty}) -> do
                     x <- lift $ readTVarIO tx
                     y <- lift $ readTVarIO ty
                     return (x,y)
              _ -> throw $ ContextException "turtle or patch" s
    (x2,y2) <- case a of
                [PatchRef (x,y) _] -> return (fromIntegral x, fromIntegral y)
                [TurtleRef _ (MkTurtle {xcor_ = tx, ycor_ = ty})] -> do
                     x <- lift $ readTVarIO tx
                     y <- lift $ readTVarIO ty
                     return (x,y)
                _ -> throw $ ContextException "turtle or patch" (head a)
    let dx' = x2 - x1
    let dy' = y2 - y1
    return $ if dx' == 0
              then
                  if dy' > 0 
                  then 0 
                  else 180
              else
                  if dy' == 0
                  then if dx' > 0 
                       then 90 
                       else 270
                  else (270 + toDegrees (pi + atan2 (-dy') dx')) `mod_` 360

  in_radius as n = do
    (_,a,_,_) <- Reader.ask
    (x, y) <- case a of
      PatchRef (x,y) _ -> return (fromIntegral x, fromIntegral y)
      TurtleRef _ (MkTurtle {xcor_ = tx, ycor_ = ty}) -> liftM2 (,) (lift $ readTVarIO tx) (lift $ readTVarIO ty)
      _ -> throw $ ContextException "turtle or patch" a
    with (distancexy x y >>= \ d -> return $ d <= n) as

  neighbors = do
    (_, a, _,  _) <- Reader.ask
    (x,y) <- case a of
      PatchRef (x,y) _ ->  return (fromIntegral x, fromIntegral y)
      TurtleRef _ (MkTurtle {xcor_ = tx, ycor_ = ty}) -> liftM2 (,) (lift $ readTVarIO tx) (lift $ readTVarIO ty)
      _ -> throw $ ContextException "turtle or patch" a
    patch_set [p (x-1) (y-1),
               p (x-1) y,
               p (x-1) (y+1),
               p x (y-1),
               p x (y+1),
               p (x+1) (y-1),
               p (x+1) y,
               p (x+1) (y+1)
              ]
    where p x y = if (not (horizontal_wrap_ conf) && (x > fromIntegral (max_pxcor_ conf) || x < fromIntegral (min_pxcor_ conf))) || (not (vertical_wrap_ conf) && (y > fromIntegral (max_pycor_ conf) || y < fromIntegral (min_pycor_ conf)))
                  then return []
                  else patch x y

  neighbors4 = do
    (_, a, _,  _) <- Reader.ask
    (x,y) <- case a of
      PatchRef (x,y) _ ->  return (fromIntegral x, fromIntegral y)
      TurtleRef _ (MkTurtle {xcor_ = tx, ycor_ = ty}) -> liftM2 (,) (lift $ readTVarIO tx) (lift $ readTVarIO ty)
      _ -> throw $ ContextException "turtle or patch" a
    patch_set [p (x-1) y,
               p (x+1) y,
               p x (y-1),
               p x (y+1)
              ]
    where p x y = if (not (horizontal_wrap_ conf) && (x > fromIntegral (max_pxcor_ conf) || x < fromIntegral (min_pxcor_ conf))) || (not (vertical_wrap_ conf) && (y > fromIntegral (max_pycor_ conf) || y < fromIntegral (min_pycor_ conf)))
                  then return []
                  else patch x y

  link x y = do
    (tw,_, _, _) <- Reader.ask
    (MkWorld _ _ ls) <- lift $ readTVarIO tw
    return [LinkRef (x,y) (ls M.! (x,y))]

  links = do
    (tw,_, _, _) <- Reader.ask
    (MkWorld _ _ ls) <- lift $ readTVarIO tw
    return $ nubBy checkForUndirected $ M.foldrWithKey (\ k x ks -> LinkRef k x: ks) [] ls
        where
          checkForUndirected (LinkRef (e1,e2) (MkLink {directed_ = False})) (LinkRef (e1',e2') (MkLink {directed_ = False})) = e1 == e2' && e1' == e2
          checkForUndirected _ _ = False

  readGlobal = lift . readTVarIO

  readTurtle i = do
    (_,a ,_,_) <- Reader.ask
    case a of
      TurtleRef _ (MkTurtle {tvars_ = pv}) -> lift $ readTVarIO (pv ! i)
      _ -> throw $ ContextException "turtle" a

  readPatch i = do 
    (_,a,_,_) <- Reader.ask
    case a of
      PatchRef _ (MkPatch {pvars_ = pv}) -> lift $ readTVarIO $ pv ! i
      TurtleRef _ _ -> patch_here >>= \ ([PatchRef _ (MkPatch {pvars_ = pv})]) -> lift $ readTVarIO $ pv !i
      _ -> throw $ ContextException "turtle or patch" a

  readLink i = do
    (_,a ,_,_) <- Reader.ask
    case a of
      LinkRef _ (MkLink {lvars_ = pv}) -> lift $ readTVarIO (pv ! i)
      _ -> throw $ ContextException "link" a

  timer = lift $ do
      t <- readTVarIO __timer
      t' <- getCurrentTime
      return $ realToFrac (t' `diffUTCTime` t)              

  reset_timer = do
      t <- lift $ getCurrentTime
      atomic $ lift $ writeTVar __timer t

  show a = do
      (_, r, p, _) <- Reader.ask
      atomic $ lift $ writeTQueue p $ (case r of
                                         ObserverRef _ -> "observer: "
                                         PatchRef (x,y) _ -> "(patch " ++ Prelude.show x ++ " " ++ Prelude.show y ++ "): "
                                         LinkRef (x,y) _ -> "(link " ++ Prelude.show x ++ " " ++ Prelude.show y ++ "): "
                                         TurtleRef i _ -> "(turtle " ++ Prelude.show i ++ "): "
                                         Nobody -> throw DevException
                                      )   ++ Prelude.show a
  print a = do
      (_, _, p, _) <- Reader.ask
      atomic $ lift $ writeTQueue p $ Prelude.show a

  ticks = lift $ readIORef __tick


-- | Reports a shade of color proportional to the value of number. 
scale_color :: (STMorIO m) => Double -> C m Double -> Double -> Double -> C m Double
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
diffuse :: CSTM Double -> (Double -> CSTM ()) -> Double -> CIO ()
diffuse gettervar settervar perc = do
  (_, a, _, _) <- Reader.ask
  case a of
    ObserverRef _ -> ask (do
                          ns <- neighbors
                          cns <- count ns
                          g <- atomic gettervar
                          let pg = g * perc / 8
                          ask (atomic (do
                               ng <- gettervar
                               settervar (ng + pg))) ns
                          atomic $ settervar (g - (pg * fromIntegral cns))
                        ) =<< patches

    _ -> throw $ ContextException "observer" a


-- Specialization trick to reduce the cost of using a class (STMorIO)
-- The downside is executable with bigger code

{-# SPECIALIZE turtles_here :: CSTM [AgentRef] #-}
{-# SPECIALIZE turtles_here :: CIO [AgentRef] #-}
{-# SPECIALIZE turtles_at :: Double -> Double -> CSTM [AgentRef] #-}
{-# SPECIALIZE turtles_at :: Double -> Double -> CIO [AgentRef] #-}
{-# SPECIALIZE patch_here :: CSTM [AgentRef] #-}
{-# SPECIALIZE patch_here :: CIO [AgentRef] #-}
{-# SPECIALIZE patches :: CSTM [AgentRef] #-}
{-# SPECIALIZE patches :: CIO [AgentRef] #-}
{-# SPECIALIZE patch :: Double -> Double -> CSTM [AgentRef] #-}
{-# SPECIALIZE patch :: Double -> Double -> CIO [AgentRef] #-}
{-# SPECIALIZE turtles :: CSTM [AgentRef] #-}
{-# SPECIALIZE turtles :: CIO [AgentRef] #-}
{-# SPECIALIZE turtle :: Int -> CSTM [AgentRef] #-}
{-# SPECIALIZE turtle :: Int -> CIO [AgentRef] #-}
{-# SPECIALIZE heading :: CSTM Double #-}
{-# SPECIALIZE heading :: CIO Double #-}
{-# SPECIALIZE xcor :: CSTM Double #-}
{-# SPECIALIZE xcor :: CIO Double #-}
{-# SPECIALIZE pcolor :: CSTM Double #-}
{-# SPECIALIZE pcolor :: CIO Double #-}
{-# SPECIALIZE ycor :: CSTM Double #-}
{-# SPECIALIZE ycor :: CIO Double #-}
{-# SPECIALIZE color :: CSTM Double #-}
{-# SPECIALIZE color :: CIO Double #-}
{-# SPECIALIZE breed :: CSTM String #-}
{-# SPECIALIZE breed :: CIO String #-}
{-# SPECIALIZE distance :: [AgentRef] -> CSTM Double #-}
{-# SPECIALIZE distance :: [AgentRef] -> CIO Double #-}
{-# SPECIALIZE distancexy :: Double -> Double -> CSTM Double #-}
{-# SPECIALIZE distancexy :: Double -> Double -> CIO Double #-}
{-# SPECIALIZE towards :: [AgentRef] -> CSTM Double #-}
{-# SPECIALIZE towards :: [AgentRef] -> CIO Double #-}
{-# SPECIALIZE in_radius :: [AgentRef] -> Double -> CSTM [AgentRef] #-}
{-# SPECIALIZE in_radius :: [AgentRef] -> Double -> CIO [AgentRef] #-}
{-# SPECIALIZE  link :: Int -> Int -> CSTM [AgentRef] #-}
{-# SPECIALIZE  link :: Int -> Int -> CIO [AgentRef] #-}
{-# SPECIALIZE links :: CSTM [AgentRef] #-}
{-# SPECIALIZE links :: CIO [AgentRef] #-}
{-# SPECIALIZE readTurtle :: Int -> CSTM Double #-}
{-# SPECIALIZE readTurtle :: Int -> CIO Double #-}
{-# SPECIALIZE readPatch :: Int -> CSTM Double #-}
{-# SPECIALIZE readPatch :: Int -> CIO Double #-}
{-# SPECIALIZE readLink :: Int -> CSTM Double #-}
{-# SPECIALIZE readLink :: Int -> CIO Double #-}
{-# SPECIALIZE timer :: CSTM Double #-}
{-# SPECIALIZE timer :: CIO Double #-}
{-# SPECIALIZE reset_timer :: CSTM () #-}
{-# SPECIALIZE reset_timer :: CIO () #-}
{-# SPECIALIZE show :: Show a => a -> CSTM () #-}
{-# SPECIALIZE show :: Show a => a -> CIO () #-}
{-# SPECIALIZE print :: Show a => a -> CSTM () #-}
{-# SPECIALIZE print :: Show a => a -> CIO () #-}
{-# SPECIALIZE ticks :: CSTM Double #-}
{-# SPECIALIZE ticks :: CIO Double #-}























