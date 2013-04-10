-- | This module tries to provide an API to the standard library of NetLogo:
-- <http://ccl.northwestern.edu/netlogo/docs/dictionary.html>
module Framework.Logo.Prim (
                           -- * Agent related
                           self, myself, other, count, distance, nobody, unsafe_distance, distancexy, unsafe_distancexy, towards, unsafe_towards, allp, at_points, towardsxy, unsafe_towardsxy, in_radius, unsafe_in_radius, in_cone, unsafe_in_cone, unsafe_every, unsafe_wait, is_agentp, carefully, is_agentsetp, die, 

                           -- * Turtle related
                           turtles_here, unsafe_turtles_here, turtles_at, unsafe_turtles_at, turtles_on, jump, setxy, forward, fd, back, bk, turtles, unsafe_turtles, turtle, unsafe_turtle, turtle_set, face, xcor, set_breed, with_breed, set_color, with_color, set_label_color, with_label_color, with_label, set_xcor, unsafe_xcor, heading, set_heading, with_heading, unsafe_heading, ycor, set_ycor, unsafe_ycor, who, color, unsafe_color, breed, unsafe_breed, dx, unsafe_dx, dy, unsafe_dy, home, right, rt, left, lt, downhill, unsafe_downhill, downhill4, unsafe_downhill4,  hide_turtle, ht, show_turtle, st, pen_down, pd, pen_up, pu, pen_erase, pe, no_turtles, is_turtlep, is_turtle_setp, hatch, move_to, set_size, with_size, with_shape,

                           -- * Patch related
                           patch_at, unsafe_patch_at, patch_here, unsafe_patch_here, patch_ahead, unsafe_patch_ahead, patches, unsafe_patches, patch, unsafe_patch, patch_set, can_movep, unsafe_can_movep, no_patches, is_patchp, is_patch_setp, pxcor, pycor,pcolor, unsafe_pcolor, neighbors, neighbors4, set_plabel, with_plabel, set_pcolor, with_pcolor, with_plabel_color,

                           -- * Link related
                           hide_link, show_link, is_linkp, is_directed_linkp, is_undirected_linkp, is_link_setp, link_length, link, unsafe_link, links, link_with, in_link_from, out_link_to, my_links, my_out_links, my_in_links, no_links, tie, untie, link_set, unsafe_links, end1, end2, 

                           -- * Random related
                           random_xcor, unsafe_random_xcor, random_ycor, unsafe_random_ycor, random_pxcor, unsafe_random_pxcor, random_pycor, unsafe_random_pycor, random, unsafe_random, random_float, unsafe_random_float, unsafe_new_seed, random_seed, unsafe_random_seed, unsafe_random_exponential, unsafe_random_gamma, unsafe_random_normal, unsafe_random_poisson,

                           -- * Color
                           primary_colors, extract_rgb, black, white, gray, red, orange, brown, yellow, green, lime, turquoise, cyan, sky, blue, violet, magenta, pink,

                           -- * List related
                           sum_, anyp, item, one_of, min_one_of, unsafe_one_of, remove, remove_item, replace_item, shuffle, unsafe_shuffle, sublist, substring, n_of, but_first, but_last, emptyp, first, foreach, fput, last_, length_, list, lput, map_, memberp, position, reduce, remove_duplicates, reverse_, sentence, sort_, sort_by, sort_on, max_, min_,n_values, is_listp, is_stringp,

                           -- * Math
                           xor, e, exp_, pi_, cos_, sin_, tan_, mod_, acos_, asin_, atan_, int, log_, mean, median, modes, variance, standard_deviation, subtract_headings, abs_, floor_, ceiling_, remainder, round_, sqrt_,  is_numberp,

                           -- * Misc
                           patch_size, max_pxcor, max_pycor, min_pxcor, min_pycor, world_width, world_height, clear_all, ca, clear_all_plots, clear_drawing, cd, clear_output, clear_turtles, ct, clear_patches, cp, clear_links, clear_ticks, reset_ticks, tick, tick_advance, ticks, unsafe_ticks, histogram, repeat_, report, loop, stop, while, 

                           -- * Input/Output
                           show_, unsafe_show_, print_, unsafe_print_, read_from_string,

                           -- * IO Operations
                           atomic, ask_, of_, with, snapshot


) where

import Framework.Logo.Base
import Framework.Logo.Conf
import Framework.Logo.Exception
import Control.Concurrent.STM
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader
import Control.Concurrent (threadDelay)
import qualified Control.Concurrent.Thread as Thread
import qualified Control.Concurrent.Thread.Group as ThreadGroup
import Data.List
import qualified Data.IntMap as IM
import qualified Data.Map as M
import Data.Array
import Control.Applicative
import System.Random hiding (random, split)
import Control.Monad (forM_)
import Data.Function
import Data.Maybe (maybe, fromJust)
import Data.Typeable
import Control.Monad (liftM, liftM2, filterM, forever, when, replicateM)
import Data.Word (Word8)
-- For diagrams

import qualified Diagrams.Prelude as Diag
import Diagrams.Backend.Postscript
import Data.Colour.SRGB (sRGB24)

-- |  Reports this turtle or patch. 
self :: Monad m => C m [AgentRef] -- ^ returns a list (set) of agentrefs to be compatible with the 'turtle-set' function
self = do
  (_, _, a, _, _,_) <- ask
  case a of
    TurtleRef _ _ -> return [a]
    PatchRef _ _ -> return [a]
    LinkRef _ _ -> return [a]
    _ -> throw (ContextException "agent" a)


-- | "self" and "myself" are very different. "self" is simple; it means "me". "myself" means "the turtle or patch who asked me to do what I'm doing right now."
-- When an agent has been asked to run some code, using myself in that code reports the agent (turtle or patch) that did the asking. 
-- NB: Implemented for ask, of, with
myself :: (Monad m) => C m [AgentRef]
myself = do
  (_,_,a,_,_,m) <- ask
  return $ case a of
             TurtleRef _ _ -> [m]
             PatchRef _ _ -> [m]
             LinkRef _ _ -> [m]
             _ -> throw (ContextException "agent" a)

-- |  Reports an agentset which is the same as the input agentset but omits this agent. 
other :: Monad m => [AgentRef] -> C m [AgentRef]
other as = do
  [s] <- self
  return $ delete s as

-- |  Reports an agentset containing all the turtles on the caller's patch (including the caller itself if it's a turtle). 
turtles_here :: CSTM [AgentRef]
turtles_here = do
  [s] <- self
  [PatchRef (px,py) _] <- case s of
                           TurtleRef _ _ -> patch_here
                           patch -> return [patch]
  ts <- turtles
  filterM (\ (TurtleRef _ (MkTurtle {xcor_ = x, ycor_ = y})) -> do 
             x' <- lift $ readTVar x
             y' <- lift $ readTVar y
             return $ round x' == px && round y' == py
          ) ts

-- |  Reports an agentset containing the turtles on the patch (dx, dy) from the caller. (The result may include the caller itself if the caller is a turtle.) 
turtles_at :: Double -> Double -> CSTM [AgentRef] -- ^ dx -> dy -> CSTM (Set AgentRef)
turtles_at x y = do
  [PatchRef (px, py) _] <- patch_at x y
  ts <- turtles
  filterM (\ (TurtleRef _ (MkTurtle {xcor_ = x, ycor_ = y})) -> do 
             x' <- lift $ readTVar x
             y' <- lift $ readTVar y
             return $ round x' == px && round y' == py
          ) ts

-- | Prints value in the Command Center, preceded by this agent, and followed by a carriage return.
show_ :: Show a => a -> CSTM ()
show_ a = do
  (_,_, r, p, _,_) <- ask
  lift $ writeTChan p $ (case r of
                           ObserverRef -> "observer: "
                           PatchRef (x,y) _ -> "(patch " ++ show x ++ " " ++ show y ++ "): "
                           LinkRef (x,y) _ -> "(link " ++ show x ++ " " ++ show y ++ "): "
                           TurtleRef i _ -> "(turtle " ++ show i ++ "): ")   ++ show a

-- | Prints value in the Command Center, followed by a carriage return. 
print_ :: Show a => a -> CSTM ()
print_ a = do
  (_,_, _, p, _,_) <- ask
  lift $ writeTChan p $ show a
                           

-- | Runs commands1. If a runtime error occurs inside commands1, NetLogo won't stop and alert the user that an error occurred. It will suppress the error and run commands2 instead. 
-- | todo
carefully :: CSTM a -> CSTM a -> CSTM a
carefully c c' = catch c (\ e -> let _ = (e :: SomeException) in c')

-- | Reports the patch at (dx, dy) from the caller, that is, the patch containing the point dx east and dy patches north of this agent. 
patch_at :: Double -> Double ->  CSTM [AgentRef]
patch_at x y = do
  (_, _, a, _, _,_) <- ask
  case a of
    PatchRef (px, py) _ -> patch (fromIntegral px) (fromIntegral py)
    TurtleRef _ _ -> do
                 [PatchRef (px, py) _] <- patch_here
                 patch (fromIntegral px + x) (fromIntegral py +y)
    _ -> throw $ ContextException "turtle or patch" a

-- | patch-here reports the patch under the turtle. 
patch_here :: CSTM [AgentRef]
patch_here = do
  (_,_, a, _, _,_) <- ask
  case a of
    TurtleRef _ (MkTurtle {xcor_ = x, ycor_ = y}) -> do
               x' <- lift $ readTVar x
               y' <- lift $ readTVar y
               patch x' y'
    _ -> throw $ ContextException "patch" a


-- | Reports the single patch that is the given distance "ahead" of this turtle, that is, along the turtle's current heading. 
patch_ahead ::Double -> CSTM [AgentRef]
patch_ahead n = do
  x <- xcor 
  y <- ycor
  dx_ <- dx
  dy_ <- dy
  let mx = fromIntegral $ max_pxcor_ conf
  let my = fromIntegral $ max_pycor_ conf
  let px_new = fromIntegral (round x) + if horizontal_wrap_ conf
                                        then (dx_*n + mx) `mod_` (truncate mx * 2 + 1) - mx
                                        else dx_*n

  let py_new = fromIntegral (round y) + if vertical_wrap_ conf
                                        then (dy_*n + my) `mod_` (truncate my * 2 + 1) - my
                                        else  dy_*n
  patch px_new py_new

-- | NetLogo Constant
black :: Double
black = 0
-- | NetLogo Constant
white :: Double
white = 9.9
-- | NetLogo Constant
gray :: Double
gray = 5
-- | NetLogo Constant
red :: Double
red = 15
-- | NetLogo Constant
orange :: Double
orange = 25
-- | NetLogo Constant
brown :: Double
brown = 35 
-- | NetLogo Constant
yellow :: Double
yellow = 45
-- | NetLogo Constant
green :: Double
green = 55
-- | NetLogo Constant
lime :: Double
lime = 65
-- | NetLogo Constant
turquoise :: Double
turquoise = 75
-- | NetLogo Constant
cyan :: Double
cyan = 85
-- | NetLogo Constant
sky :: Double
sky = 95
-- | NetLogo Constant
blue :: Double
blue = 105
-- | NetLogo Constant
violet :: Double
violet = 115
-- | NetLogo Constant
magenta :: Double
magenta = 125
-- | NetLogo Constant
pink :: Double
pink = 135

-- approximate-rgb

-- | Internal
primary_colors :: [Double]
primary_colors = [gray, red, orange, brown, yellow, green, lime, turquoise, cyan, sky, blue, violet, magenta, pink]

-- | Reports the number of agents in the given agentset. 
count :: Monad m => [AgentRef] -> C m Int
count [Nobody] = throw $ TypeException "agent" Nobody
count as = return $ length as

-- | Reports true if the given agentset is non-empty, false otherwise. 
anyp :: Monad m => [AgentRef] -> C m Bool
anyp [Nobody] = throw $ TypeException "agent" Nobody
anyp as = return $ not $ null as

allp :: CIO Bool -> [AgentRef] -> CIO Bool
allp _ [] = return True
allp r as = do
  res <- with r as
  return $ if length as == length res
           then True
           else False

-- | Reports the agentset consisting of all patches. 
patches :: CSTM [AgentRef]
patches = do
  (_,tw,_, _, _,_) <- ask
  (MkWorld ps _ _) <- lift $ readTVar tw
  return $ M.foldrWithKey (\ k x ks -> PatchRef k x: ks) [] ps



-- | Given the x and y coordinates of a point, reports the patch containing that point. 
patch :: Double -> Double -> CSTM [AgentRef]
patch x y = do
  (_, tw,_, _, _,_) <- ask
  (MkWorld ps _ _) <- lift $ readTVar tw
  return $ if (not (horizontal_wrap_ conf) && (x' > max_pxcor_ conf || x' < min_pxcor_ conf)) || (not (vertical_wrap_ conf) && (y' > max_pycor_ conf || y' < min_pycor_ conf))
           then [Nobody]
           else
               [PatchRef (x'', y'') (ps M.! (x'', y''))]
         where
           x' = round x
           y' = round y
           (x'',y'') = normalize x' y'
           normalize :: Int -> Int -> (Int, Int)
           normalize x y = (
                            ((x + max_pxcor_ conf) `mod` (max_pxcor_ conf*2+1)) - max_pxcor_ conf,
                            ((y + max_pycor_ conf) `mod` (max_pycor_ conf*2+1)) - max_pycor_ conf
                           )




-- | The turtle moves forward by number units all at once (rather than one step at a time as with the forward command). 
jump :: Double -> CSTM ()
jump n = do
  (_,_, a, _, _,_) <- ask
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
  (_,_, a, _, _,_) <- ask
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
                      if (dmin_x -0.5 < x' && x' < dmax_x + 0.5) 
                      then lift $ writeTVar tx x'
                      else error "wrap"
                if vertical_wrap_ conf
                  then
                      lift $ writeTVar ty $ ((y' + dmax_y) `mod_` (max_y + abs min_y +1)) + dmin_y
                  else
                      if (dmin_y -0.5  < y' && y' < dmax_y + 0.5) 
                      then lift $ writeTVar ty y'
                      else error "wrap"
    _ -> throw $ ContextException "turtle" a


-- | The turtle moves forward by number steps, one step at a time. (If number is negative, the turtle moves backward.) 
forward :: Double -> CSTM ()
forward n | n == 0 = do
  (_,_, a, _, _,_) <- ask
  case a of
    TurtleRef _ _ -> return ()
    _ -> throw $ ContextException "turtle" a
          | n > 1 = jump 1 >> forward (n-1)
          | n < -1 = jump (-1) >> forward (n+1)
          | (0 < n && n <= 1) || (-1 <= n && n < 0) = jump n
{-# INLINE fd #-}
-- | alias for 'forward'
fd = forward

-- | The turtle moves backward by number steps. (If number is negative, the turtle moves forward.) 
{-# INLINE back #-}
back :: Double -> CSTM ()
back n = forward (-n)
{-# INLINE bk #-}
-- | alias for 'back'
bk = back

-- {-# INLINE crt #-}
-- -- | alias for 'create_turtles'
-- crt = create_turtles


-- {-# INLINE cro #-}
-- -- | alias for 'create_ordered_turtles'
-- cro = create_ordered_turtles

die :: CSTM ()
die = do
 (_,tw,a,_,_,_) <- ask
 case a of
   TurtleRef t _ -> do
          (MkWorld ps ts ls) <- lift $ readTVar tw
          lift $ writeTVar tw $ MkWorld ps (IM.delete t ts) ls
   PatchRef p _ -> do
          (MkWorld ps ts ls) <- lift $ readTVar tw
          lift $ writeTVar tw $ MkWorld (M.delete p ps) ts ls
   LinkRef (e1,e2) (MkLink {directed_ = d}) -> do
          (MkWorld ps ts ls) <- lift $ readTVar tw
          lift $ writeTVar tw $ MkWorld ps ts (M.delete (e1,e2) 
                                                (if d -- is directed
                                                then ls
                                                else M.delete (e2,e1) ls
                                                )
                                              )
   _ -> throw $ ContextException "turtle or patch" a

-- | Reports the agentset consisting of all turtles. 
turtles :: CSTM [AgentRef]
turtles = do
  (_,tw,_, _, _,_) <- ask
  MkWorld _ ts _ <- lift $ readTVar tw
  return $ IM.foldrWithKey (\ k x ks -> TurtleRef k x: ks) [] ts

-- | Reports the turtle with the given who number, or nobody if there is no such turtle. For breeded turtles you may also use the single breed form to refer to them. 
turtle :: Int -> CSTM [AgentRef]
turtle n = do
  (_, tw,_, _, _,_) <- ask
  (MkWorld _ ts _) <- lift $ readTVar tw
  return $ maybe [Nobody] (return . TurtleRef n) $ IM.lookup n ts


-- | Reports an agentset containing all of the turtles anywhere in any of the inputs.
-- | NB: HLogo no support for nested turtle_set concatenation/flattening
turtle_set :: Monad m => [C m [AgentRef]] -> C m [AgentRef]
turtle_set ts = sequence ts >>= return . foldr (\ x acc -> 
                                                if x == Nobody -- filter Nobody
                                                then acc
                                                else case x of -- type check
                                                       TurtleRef _ _ -> if x `elem` acc -- nub
                                                                      then acc
                                                                      else (x:acc)
                                                       _ -> throw $ TypeException "turtle" x
                                               ) [] . concat

-- | Reports an agentset containing all of the patches anywhere in any of the inputs.
-- | NB: HLogo no support for nested turtle_set concatenation/flattening
patch_set :: Monad m => [C m [AgentRef]] -> C m [AgentRef]
patch_set ts = sequence ts >>= return . foldr (\ x acc -> 
                                                if x == Nobody -- filter Nobody
                                                then acc
                                                else case x of -- type check
                                                       PatchRef _ _ -> if x `elem` acc -- nub
                                                                      then acc
                                                                      else (x:acc)
                                                       _ -> throw $ TypeException "patch" x
                                               ) [] . concat


-- | Reports true if this turtle can move distance in the direction it is facing without violating the topology; reports false otherwise. 
can_movep :: Double -> CSTM Bool
can_movep n = patch_ahead n >>= return . ( /= [Nobody])


-- | This is a built-in turtle variable. It indicates the direction the turtle is facing. 
heading :: CSTM Double
heading = do
  (_,_,a, _, _,_) <- ask
  case a of
    TurtleRef _ (MkTurtle {heading_ = h}) -> lift $ readTVar h
    _ -> throw (ContextException "turtle" a)

set_heading :: Double -> CSTM ()
set_heading v = do
  (_,_,a, _, _,_) <- ask
  case a of
    TurtleRef _ t -> lift $ writeTVar (heading_ t) v
    _ -> throw $ ContextException "turtle" a

-- | This is a built-in turtle variable. It holds the current x coordinate of the turtle. 
xcor :: CSTM Double
xcor = do
  (_,_,a, _, _,_) <- ask
  case a of
    TurtleRef _ (MkTurtle {xcor_ = x}) -> lift $ readTVar x
    _ -> throw $ ContextException "turtle" a

-- |These are built-in patch variables. They hold the x and y coordinate of the patch. They are always integers. You cannot set these variables, because patches don't move. 
pxcor :: (Monad m) => C m Int
pxcor = do
  (_,_,a, _, _,_) <- ask
  case a of
    PatchRef _ (MkPatch {pxcor_ = x}) -> return x
    _ -> throw $ ContextException "patch" a

-- | These are built-in patch variables. They hold the x and y coordinate of the patch. They are always integers. You cannot set these variables, because patches don't move. 
pycor :: (Monad m) => C m Int
pycor = do
  (_,_,a, _, _,_) <- ask
  case a of
    PatchRef _ (MkPatch {pycor_ = y}) -> return y
    _ -> throw $ ContextException "patch" a

pcolor :: CSTM Double
pcolor = do
  (_,_,a, _, _,_) <- ask
  case a of
    PatchRef _ (MkPatch {pcolor_ = tc}) -> lift $ readTVar tc
    TurtleRef _ _ -> do
           [PatchRef _ (MkPatch {pcolor_ = tc})] <- patch_here
           lift $ readTVar tc
    _ -> throw $ ContextException "turtle or patch" a


unsafe_pcolor :: CIO Double
unsafe_pcolor = do
  (_,_,a, _, _,_) <- ask
  case a of
    PatchRef _ (MkPatch {pcolor_ = tc}) -> lift $ readTVarIO tc
    TurtleRef _ _ -> do
           [PatchRef _ (MkPatch {pcolor_ = tc})] <- unsafe_patch_here
           lift $ readTVarIO tc
    _ -> throw $ ContextException "patch" a

-- | Reports an agentset containing the 8 surrounding patches
neighbors :: CSTM [AgentRef]
neighbors = do
  (_, _, a, _, _, _) <- ask
  case a of
    PatchRef (x,y) _ ->  patch_set [p (x-1) (y-1),
                                   p (x-1) y,
                                   p (x-1) (y+1),
                                   p x (y-1),
                                   p x (y+1),
                                   p (x+1) (y-1),
                                   p (x+1) y,
                                   p (x+1) (y+1)
                                  ]
    _ -> throw $ ContextException "patch" a
  where p x y = if (not (horizontal_wrap_ conf) && (x > max_pxcor_ conf || x < min_pxcor_ conf)) || (not (vertical_wrap_ conf) && (y > max_pycor_ conf || y < min_pycor_ conf))
                then return []
                else patch (fromIntegral x) (fromIntegral y)

-- | Reports an agentset containing the 4 surrounding patches
neighbors4 :: CSTM [AgentRef]
neighbors4 = do
  (_, _, a, _, _, _) <- ask
  case a of
    PatchRef (x,y) _ ->  patch_set [p (x-1) y,
                                   p (x+1) y,
                                   p x (y-1),
                                   p x (y+1)
                                  ]
    _ -> throw $ ContextException "patch" a
  where p x y = if (not (horizontal_wrap_ conf) && (x > max_pxcor_ conf || x < min_pxcor_ conf)) || (not (vertical_wrap_ conf) && (y > max_pycor_ conf || y < min_pycor_ conf))
                then return []
                else patch (fromIntegral x) (fromIntegral y)

set_plabel :: String -> CSTM ()
set_plabel s = do
  (_,_,a,_,_,_) <- ask
  case a of
    PatchRef _ (MkPatch {plabel_ = p}) -> lift $ writeTVar p s
    _ -> throw $ ContextException "patch" a

set_pcolor :: Double -> CSTM ()
set_pcolor s = do
  (_,_,a,_,_,_) <- ask
  case a of
    PatchRef _ (MkPatch {pcolor_ = tc}) -> lift $ writeTVar tc s
    TurtleRef _ _ -> do
                 [PatchRef _ (MkPatch {pcolor_ = tc})] <- patch_here
                 lift $ writeTVar tc s
    _ -> throw $ ContextException "turtle or patch" a

set_breed :: String -> CSTM ()
set_breed v = do
  (_,_,a, _, _,_) <- ask
  case a of
    TurtleRef _ t -> lift $ writeTVar (breed_ t) v
    _ -> throw $ ContextException "turtle" a

set_color :: Double -> CSTM ()
set_color v = do
  (_,_,a, _, _,_) <- ask
  case a of
    TurtleRef _ t -> lift $ writeTVar (color_ t) v
    LinkRef _ (MkLink {lcolor_ = c}) -> lift $ writeTVar c v
    _ -> throw $ ContextException "turtle or link" a

set_label_color :: Double -> CSTM ()
set_label_color v = do
  (_,_,a, _, _,_) <- ask
  case a of
    TurtleRef _ t -> lift $ writeTVar (label_color_ t) v
    LinkRef _ (MkLink {llabel_color_ = c}) -> lift $ writeTVar c v
    _ -> throw $ ContextException "turtle or link" a


set_xcor :: Double -> CSTM ()
set_xcor x' = do
  (_,_,a, _, _,_) <- ask
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
                     if (dmin_x -0.5 < x' && x' < dmax_x + 0.5) 
                     then lift $ writeTVar tx x'
                     else error "wrap"
    _ -> throw $ ContextException "turtle" a


set_size :: Double -> CSTM ()
set_size v = do
  (_,_,a, _, _,_) <- ask
  case a of
    TurtleRef _ t -> lift $ writeTVar (size_ t) v
    _ -> throw $ ContextException "turtle" a


-- | This is a built-in turtle variable. It holds the current y coordinate of the turtle.
ycor :: CSTM Double
ycor = do
  (_,_,a, _, _,_) <- ask
  case a of
    TurtleRef _ (MkTurtle {ycor_ = y}) -> lift $ readTVar y
    _ -> throw $ ContextException "turtle" a

set_ycor :: Double -> CSTM ()
set_ycor y' = do
  (_,_,a, _, _,_) <- ask
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
                     if (dmin_y -0.5 < y' && y' < dmax_y + 0.5) 
                     then lift $ writeTVar ty y'
                     else error "wrap"
    _ -> throw $ ContextException "turtle" a

-- | This is a built-in turtle variable. It holds the turtle's "who number" or ID number, an integer greater than or equal to zero. You cannot set this variable; a turtle's who number never changes. 
who :: Monad m => C m Int
who = do
  (_,_,a, _, _,_) <- ask
  case a of
    TurtleRef i _ -> return i
    _ -> throw $ ContextException "turtle" a

-- | This is a built-in turtle variable. It holds the turtle's "who number" or ID number, an integer greater than or equal to zero. You cannot set this variable; a turtle's who number never changes. 
color :: CSTM Double
color = do
  (_,_,a, _, _,_) <- ask
  case a of
    TurtleRef _ (MkTurtle {color_ = c}) -> lift $ readTVar c
    LinkRef _ (MkLink {lcolor_ = c}) -> lift $ readTVar c
    _ -> throw $ ContextException "turtle or link" a


breed :: CSTM String
breed = do
  (_,_,a, _, _,_) <- ask
  case a of
    TurtleRef _ (MkTurtle {breed_ = b}) -> lift $ readTVar b
    LinkRef _ (MkLink {lbreed_ = b}) -> return b
    _ -> throw $ ContextException "turtle or link" a

unsafe_breed :: CIO String
unsafe_breed = do
  (_,_,a, _, _,_) <- ask
  case a of
    TurtleRef _ (MkTurtle {breed_ = b}) -> lift $ readTVarIO b
    LinkRef _ (MkLink {lbreed_ = b}) -> return b
    _ -> throw $ ContextException "turtle or link" a

-- | Reports the x-increment (the amount by which the turtle's xcor would change) if the turtle were to take one step forward in its current heading. 
dx :: CSTM Double
dx = liftM sin_ heading

-- | Reports the y-increment (the amount by which the turtle's ycor would change) if the turtle were to take one step forward in its current heading. 
dy :: CSTM Double
dy = liftM cos_ heading

random_seed :: Int -> CSTM ()
random_seed i = do
  (_,_,_,_,tgen,_) <- ask
  lift $ writeTVar tgen (mkStdGen i)
                
-- | Reports a random floating point number from the allowable range of turtle coordinates along the given axis, x . 
random_xcor :: CSTM Double
random_xcor = do
  (_,_,_,_,ts,_) <- ask
  s <- lift $ readTVar ts
  let (v, s') = randomR ((fromIntegral $ min_pxcor_ conf),(fromIntegral $ max_pxcor_ conf)) s
  lift $ writeTVar ts s'
  return v

-- | Reports a random floating point number from the allowable range of turtle coordinates along the given axis, y. 
random_ycor :: CSTM Double
random_ycor = do
  (_,_,_,_,ts,_) <- ask
  s <- lift $ readTVar ts
  let (v, s') = randomR ((fromIntegral $ min_pycor_ conf),(fromIntegral $ max_pycor_ conf)) s
  lift $ writeTVar ts s'
  return v

-- | Reports a random integer ranging from min-pxcor to max-pxcor inclusive. 
random_pxcor :: CSTM Int
random_pxcor = do
  (_,_,_,_,ts,_) <- ask
  s <- lift $ readTVar ts
  let (v, s') = randomR (min_pxcor_ conf, max_pxcor_ conf) s
  lift $ writeTVar ts s'
  return v

-- | Reports a random integer ranging from min-pycor to max-pycor inclusive. 
random_pycor :: CSTM Int
random_pycor = do
  (_,_,_,_,ts,_) <- ask
  s <- lift $ readTVar ts
  let (v, s') = randomR (min_pycor_ conf, max_pycor_ conf) s
  lift $ writeTVar ts s'
  return v

-- | If number is positive, reports a random integer greater than or equal to 0, but strictly less than number.
-- If number is negative, reports a random integer less than or equal to 0, but strictly greater than number.
-- If number is zero, the result is always 0 as well. 
random               :: (Random a , Eq a, Ord a, Num a) => a -> CSTM a
random x | x == 0     = return 0
         | otherwise = do
  (_,_,_,_,ts,_) <- ask
  s <- lift $ readTVar ts
  let (v, s') = randomR (if x < 0 then (x, 0) else (0,x)) s
  lift $ writeTVar ts s'
  return v

-- |  If number is positive, reports a random floating point number greater than or equal to 0 but strictly less than number.
-- If number is negative, reports a random floating point number less than or equal to 0, but strictly greater than number.
-- If number is zero, the result is always 0. 
random_float               :: Double -> CSTM Double
random_float x | x == 0     = return 0
         | otherwise = do
  (_,_,_,_,ts,_) <- ask
  s <- lift $ readTVar ts
  let (v, s') = randomR (if x < 0 then (x, 0) else (0,x)) s
  lift $ writeTVar ts s'
  return v

-- | This turtle moves to the origin (0,0). Equivalent to setxy 0 0. 
home :: CSTM ()
home = setxy 0 0

-- | The turtle turns right by number degrees. (If number is negative, it turns left.) 
right :: Double -> CSTM ()
right n = do
  (_, _, a, _, _ ,_) <- ask
  case a of
    TurtleRef _ t -> lift $ modifyTVar' (heading_ t) (\ h -> mod_ (h+n) 360)

{-# INLINE rt #-}
-- | alias for 'right'
rt = right

-- | The turtle turns left by number degrees. (If number is negative, it turns right.) 
left :: Double -> CSTM ()
left n = do
  right (-n)

{-# INLINE lt #-}
-- | alias for 'left'
lt = left

-- | Reports the distance from this agent to the given turtle or patch. 
distance :: [AgentRef] -> CSTM Double
distance [PatchRef (x,y) _] = do
  distancexy (fromIntegral x) (fromIntegral y)
distance [TurtleRef _ (MkTurtle {xcor_ = tx, ycor_ = ty})] = do
  x <- lift $ readTVar tx
  y <- lift $ readTVar ty
  distancexy x y
distance [a] = throw $ TypeException "turtle or patch" a

delta a1 a2 aboundary =
    min (abs (a2 - a1)) (abs (a2 + a1) + 1)

-- | Reports the distance from this agent to the point (xcor, ycor). 
distancexy :: Double -> Double -> CSTM Double
distancexy x' y' = do
  (_,_,a,_,_,_) <- ask
  (x,y) <- case a of
            PatchRef (x,y) _ -> return (fromIntegral x, fromIntegral y)
            TurtleRef _ (MkTurtle {xcor_ = tx, ycor_ = ty}) -> liftM2 (,) (lift $ readTVar tx) (lift $ readTVar ty)
            _ -> throw $ ContextException "turtle or patch" a
  return $ sqrt ((delta x x' (fromIntegral $ max_pxcor_ conf)) ^ 2 + (delta y y' (fromIntegral $ max_pycor_ conf) ^ 2))


-- | This is a special value which some primitives such as turtle, one-of, max-one-of, etc. report to indicate that no agent was found. Also, when a turtle dies, it becomes equal to nobody. 
nobody :: Monad m => m [AgentRef]
nobody = return [Nobody]

-- | Moves the turtle to the neighboring patch with the lowest value for patch-variable. 
-- If no neighboring patch has a smaller value than the current patch, the turtle stays put. 
-- If there are multiple patches with the same lowest value, the turtle picks one randomly. 
-- Non-numeric values are ignored.
-- downhill considers the eight neighboring patches
-- | todo
downhill = undefined

-- | Moves the turtle to the neighboring patch with the lowest value for patch-variable. 
-- If no neighboring patch has a smaller value than the current patch, the turtle stays put. 
-- If there are multiple patches with the same lowest value, the turtle picks one randomly. 
-- Non-numeric values are ignored.
-- downhill4 only considers the four neighbors. 
-- | todo
downhill4 = undefined

-- | Set the caller's heading towards agent. 
face :: [AgentRef] -> CSTM ()
face a = set_heading =<< towards a

-- | Reports the heading from this agent to the given agent. 
-- | todo: wrapping
towards :: [AgentRef] -> CSTM Double
towards a = do
  (_, _, s, _, _, _) <- ask
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
  let dx = x2 - x1
  let dy = y2 - y1
  return $ if (dx == 0)
            then
                if dy > 0 
                then 0 
                else 180
            else
                if (dy == 0)
                then if dx > 0 
                     then 90 
                     else 270
                else (270 + toDegrees (pi + atan2 (-dy) dx)) `mod_` 360

-- | Reports the heading from the turtle or patch towards the point (x,y). 
-- | todo
towardsxy = undefined


-- | The turtle makes itself invisible. 
hide_turtle :: CSTM ()
hide_turtle = do
  (_,_,a, _, _,_) <- ask
  case a of
    TurtleRef _ (MkTurtle {hiddenp_ = th}) -> lift $ writeTVar th True
    _ -> throw $ ContextException "turtle" a

{-# INLINE ht #-}
-- | alias for 'hide_turtle'
ht = hide_turtle

-- | The turtle becomes visible again. 
show_turtle :: CSTM ()
show_turtle = do
  (_,_,a, _, _,_) <- ask
  case a of
    TurtleRef _ (MkTurtle {hiddenp_ = th}) -> lift $ writeTVar th False
    _ -> throw $ ContextException "turtle" a

{-# INLINE st #-}
-- | alias for 'show_turtle'
st = show_turtle

-- | The turtle changes modes between drawing lines, removing lines or neither. 
pen_down :: CSTM ()
pen_down = do
  (_,_,a, _, _,_) <- ask
  case a of
    TurtleRef _ (MkTurtle {pen_mode_ = tp}) -> lift $ writeTVar tp Down
    _ -> throw $ ContextException "turtle" a

{-# INLINE pd #-}
-- | alias for 'pen_down'
pd = pen_down

-- | The turtle changes modes between drawing lines, removing lines or neither. 
pen_up :: CSTM ()
pen_up = do
  (_,_,a, _, _,_) <- ask
  case a of
    TurtleRef _ (MkTurtle {pen_mode_ = tp}) -> lift $ writeTVar tp Up
    _ -> throw $ ContextException "turtle" a

{-# INLINE pu #-}
-- | alias for 'pen_up'
pu = pen_up

pen_erase :: CSTM ()
-- | The turtle changes modes between drawing lines, removing lines or neither. 
pen_erase = do
  (_,_,a, _, _,_) <- ask
  case a of
    TurtleRef _ (MkTurtle {pen_mode_ = tp}) -> lift $ writeTVar tp Erase
    _ -> throw $ ContextException "turtle" a

{-# INLINE pe #-}
-- | alias for 'pen_erase'
pe = pen_erase

-- | Reports an agentset that includes only those agents from the original agentset whose distance from the caller is less than or equal to number. (This can include the agent itself.) 
in_radius :: [AgentRef] -> Double -> CSTM [AgentRef]
in_radius as n = do
  (_, _, a, _, _,_) <- ask
  (x, y) <- case a of
             PatchRef (x,y) _ -> return $ (fromIntegral x, fromIntegral y)
             TurtleRef _ (MkTurtle {xcor_ = tx, ycor_ = ty}) -> liftM2 (,) (lift $ readTVar tx) (lift $ readTVar ty)
             _ -> throw $ ContextException "turtle or patch" a
  filterM (\ (TurtleRef _ (MkTurtle {xcor_ = tx', ycor_ = ty'})) -> do 
             x' <- lift $ readTVar tx'
             y' <- lift $ readTVar ty'
             return $ (sqrt ((delta x x' (fromIntegral $ max_pxcor_ conf)) ^ 2 + (delta y y' (fromIntegral $ max_pycor_ conf) ^ 2))) <= n) as

-- | This reporter lets you give a turtle a "cone of vision" in front of itself. 
in_cone = undefined


-- | Reports an empty turtle agentset. 
no_turtles :: (Monad m) => C m [AgentRef]
no_turtles = return []

-- | Reports an empty patch agentset. 
no_patches :: (Monad m) => C m [AgentRef]
no_patches = return []

-- | Reports true if either boolean1 or boolean2 is true, but not when both are true. 
xor p q = (p || q) && not (p && q)


patch_size :: Monad m => C m Int
patch_size = return $ patch_size_ conf

-- | This reporter gives the maximum x-coordinate for patches, which determines the size of the world. 
max_pxcor :: Monad m => C m Int
max_pxcor = return $ max_pxcor_ conf

-- | This reporter gives the maximum y-coordinate for patches, which determines the size of the world. 
max_pycor :: (Monad m) => C m Int
max_pycor = return $ max_pycor_ conf

-- | This reporter gives the minimum x-coordinate for patches, which determines the size of the world. 
min_pxcor :: (Monad m ) => C m Int
min_pxcor = return $ min_pxcor_ conf

-- | This reporter gives the maximum y-coordinate for patches, which determines the size of the world. 
min_pycor :: (Monad m) => C m Int
min_pycor = return $ min_pycor_ conf

-- | This reporter gives the total width of the NetLogo world. 
world_width :: (Monad m) => C m Int
world_width = return $ (max_pxcor_ conf) - (min_pxcor_ conf) + 1

-- | This reporter gives the total height of the NetLogo world. 
world_height :: (Monad m) => C m Int
world_height = return $ (max_pycor_ conf) - (min_pycor_ conf) + 1


-- | Resets all global variables to zero, and calls clear-ticks, clear-turtles, clear-patches, clear-drawing, clear-all-plots, and clear-output. 
clear_all :: CSTM ()
clear_all = do
  clear_ticks
  clear_turtles
  clear_patches
  clear_drawing
  clear_all_plots
  clear_output

{-# INLINE ca #-}
-- | alias for 'clear_all'
ca = clear_all

-- | Clears every plot in the model.
-- | todo
clear_all_plots = do
    (_, _, a, _, _,_) <- ask
    case a of
      ObserverRef -> return ()
      _ -> throw $ ContextException "observer" a

-- | Clears all lines and stamps drawn by turtles. 
-- | todo
clear_drawing = do
    (_, _, a, _, _,_) <- ask
    case a of
      ObserverRef -> return ()
      _ -> throw $ ContextException "observer" a

{-# INLINE cd #-}
-- | alias for 'clear_drawing'
cd = clear_drawing

-- | Clears all text from the model's output area, if it has one. Otherwise does nothing. 
-- | todo
clear_output = do
    (_, _, a, _, _,_) <- ask
    case a of
      ObserverRef -> return ()
      _ -> throw $ ContextException "observer" a


-- | Kills all turtles.
-- Also resets the who numbering, so the next turtle created will be turtle 0.
clear_turtles :: CSTM ()
clear_turtles = do
  (_, tw, a, _, _,_) <- ask
  case a of
    ObserverRef -> do
                  (MkWorld ps _ ls) <- lift $ readTVar tw
                  lift $ writeTVar tw (MkWorld ps IM.empty ls)
    _ -> throw $ ContextException "observer" a

{-# INLINE ct #-}
-- | alias for 'clear_turtles'
ct = clear_turtles

-- | Kills all links.
clear_links :: CSTM ()
clear_links = do
  (_, tw, a, _, _,_) <- ask
  case a of
    ObserverRef -> do
                (MkWorld ps ts _) <- lift $ readTVar tw
                lift $ writeTVar tw (MkWorld ps ts M.empty)
    _ -> throw $ ContextException "observer" a

-- | Clears the patches by resetting all patch variables to their default initial values, including setting their color to black. 
clear_patches :: CSTM ()
clear_patches = do
  (_, tw, a, _, _,_) <- ask
  case a of
    ObserverRef -> do
                  (MkWorld ps ts _) <- lift $ readTVar tw
                  lift $ M.traverseWithKey (\ (x,y) (MkPatch tx ty tc tl tlc to)  -> do
                              writeTVar tc 0
                              writeTVar tl ""
                              writeTVar tlc 9.9
                              mapM_ (flip writeTVar 0) (elems to) -- patches-own to 0
                           ) ps
                  return ()
    _ -> throw $ ContextException "observer" a

{-# INLINE cp #-}
-- | alias for 'clear_patches'
cp = clear_patches


-- | Clears the tick counter.
-- Does not set the counter to zero. After this command runs, the tick counter has no value. Attempting to access or update it is an error until reset-ticks is called. 
clear_ticks :: CSTM ()
clear_ticks = do
    (gs, _, a, _, _,_) <- ask
    case a of
      ObserverRef -> lift $ writeTVar (gs ! 1) undefined
      _ -> throw $ ContextException "observer" a

-- | Resets the tick counter to zero, sets up all plots, then updates all plots (so that the initial state of the world is plotted). 
reset_ticks :: CSTM ()
reset_ticks = do
    (gs, _, a, _, _,_) <- ask
    case a of
      ObserverRef -> lift $ writeTVar (gs ! 1) 0
      _ -> throw $ ContextException "observer" a

-- | Advances the tick counter by one and updates all plots. 
tick :: CSTM ()
tick = tick_advance 1

-- | Advances the tick counter by number. The input may be an integer or a floating point number. (Some models divide ticks more finely than by ones.) The input may not be negative. 
-- todo: dynamic typing, float
tick_advance :: Double -> CSTM ()
tick_advance n = do
  (gs, _, a, _, _,_) <- ask
  case a of
    ObserverRef -> lift $ modifyTVar' (gs ! 1) (+n)
    _ -> throw $ ContextException "observer" a

-- | Reports the current value of the tick counter. The result is always a number and never negative. 
-- todo: dynamic typing, integer or float
ticks :: CSTM Double
ticks = do
  (gs, _, _, _, _,_) <- ask
  lift $ readTVar (gs ! 1)

{-# INLINE but_first #-}
-- | When used on a list, but-first reports all of the list items of list except the first
but_first = tail

{-# INLINE but_last #-}
-- | but-last reports all of the list items of list except the last. 
but_last = init

{-# INLINE emptyp #-}
-- | Reports true if the given list or string is empty, false otherwise. 
emptyp = null

{-# INLINE first #-}
-- | On a list, reports the first (0th) item in the list. 
first = head

{-# INLINE foreach #-}
-- | With a single list, runs the task for each item of list. 
foreach :: Monad m => [a] -> (a -> m b) -> m ()
foreach = forM_

{-# INLINE fput #-}
-- | Adds item to the beginning of a list and reports the new list. 
fput = (:)

-- | Histograms the values in the given list
-- Draws a histogram showing the frequency distribution of the values in the list. The heights of the bars in the histogram represent the numbers of values in each subrange. 
-- | todo
histogram = undefined

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
item i l = l !! (i-1)

{-# INLINE last_ #-}
-- | On a list, reports the last item in the list. 
last_ = last

{-# INLINE length_ #-}
-- | Reports the number of items in the given list, or the number of characters in the given string. 
length_ = length

{-# INLINE list #-}
-- | Reports a list containing the given items.
list x y = [x,y]

{-# INLINE lput #-}
-- | Adds value to the end of a list and reports the new list. 
lput x l = l ++ [x]

{-# INLINE map_ #-}
-- | With a single list, the given task is run for each item in the list, and a list of the results is collected and reported. 
map_ = map

{-# INLINE memberp #-}
-- | For a list, reports true if the given value appears in the given list, otherwise reports false. 
memberp :: Eq a => a -> [a] -> Bool
memberp = elem


-- | Reports a list of length size containing values computed by repeatedly running the task. 
n_values 0 _ = return []
n_values s f = do
    h <- f s 
    t <- n_values (s-1) f
    return (h:t)

-- @doc requires distinction between list and string dt, because it behaves differently
-- list: returns the index of the 1st occurence or false otherwise
-- string: returns the index of the 1st substring that matches the string or false otherwise
-- here it is implemented only with the list behaviour
-- @todo string behaviour
-- 0-indexed
-- no dynamic typing, so can't return false

-- | todo: requires dynamic typing
{-# INLINE position #-}
-- | On a list, reports the first position of item in list, or false if it does not appear. 
position = find

-- |  From an agentset, reports a random agent. If the agentset is empty, reports nobody.
-- From a list, reports a random list item. It is an error for the list to be empty. 
one_of :: [a] -> CSTM [a]
one_of [] = error "empty list"
one_of l = do
  (_,_,_,_,ts,_) <- ask
  s <- lift $ readTVar ts
  let (v,s') = randomR (0, length l -1) s
  lift $ writeTVar ts s'
  return [l !! v]

-- Uses instead agent_one_of when types match
{-# RULES "one_of/AgentRef" one_of = agent_one_of #-}

agent_one_of :: [AgentRef] -> CSTM [AgentRef]
agent_one_of [] = nobody
agent_one_of l = do
  (_,_,_,_,ts,_) <- ask
  s <- lift $ readTVar ts
  let (v,s') = randomR (0, length l -1) s
  lift $ writeTVar ts s'
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


-- | Reports a random agent in the agentset that reports the lowest value for the given reporter. If there is a tie, this command reports one random agent that meets the condition.
-- | todo: currently deterministic and no randomness on tie breaking
min_one_of :: Ord a => [AgentRef] -> CIO a -> CIO [AgentRef]
min_one_of as r = do
  rs <- of_ r as
  return [snd $ minimum $ zip rs as]



{-# INLINE reduce #-}
-- | Reduces a list from left to right using the given task, resulting in a single value. (foldl)
reduce = foldl

-- | For a list, reports a copy of list with all instances of item removed. 
-- | todo
remove = undefined

{-# INLINE remove_duplicates #-}
-- | Reports a copy of list with all duplicate items removed. The first of each item remains in place. 
remove_duplicates :: (Eq a, Monad m) => [a] -> C m [a]
remove_duplicates = return . nub

-- | For a list, reports a copy of list with the item at the given index removed. 
-- | todo
remove_item = undefined

-- | On a list, replaces an item in that list. index is the index of the item to be replaced, starting with 0. 
-- | todo
replace_item = undefined

{-# INLINE reverse_ #-}
-- | Reports a reversed copy of the given list or string. 
reverse_ = reverse

{-# INLINE sentence #-}
-- | Makes a list out of the values. 
-- | todo: requires dynamic_typing
sentence = (++)

-- | Reports a new list containing the same items as the input list, but in randomized order. 
-- | todo optimize with arrays <http://www.haskell.org/haskellwiki/Random_shuffle>
shuffle :: Eq a => [a] -> CSTM [a]
shuffle [] = return []
shuffle l = shuffle' l (length l) where
    shuffle [x] 1 = return [x]
    shuffle' l i = do
      [x] <- one_of l
      xs <- shuffle' (delete x l) (i-1)
      return $ x:xs

{-# INLINE sort_ #-}
-- | Reports a sorted list of numbers, strings, or agents. 
sort_ :: (Monad m, Ord a) => [a] -> m [a]
sort_ = return . sort

{-# INLINE sort_by #-}
-- | If the input is a list, reports a new list containing the same items as the input list, in a sorted order defined by the boolean reporter task. 
-- | todo: requires dynamic typing
sort_by :: Monad m => (a -> a -> Ordering) -> [a] -> m [a]
sort_by c l = return $ sortBy c l

-- | Reports a list of agents, sorted according to each agent's value for reporter. Ties are broken randomly. 
sort_on :: Ord a => CSTM a -> [AgentRef] -> CSTM [AgentRef]
sort_on rep as = do
  (gs, tw, s, p, g,_) <- ask
  xs <- lift . sequence $ [runReaderT rep (gs, tw, a, p, g, s) | a <- as]
  let rs = zip xs as
  return $ map snd $ sortBy (compare `on` fst) rs where


-- | Reports just a section of the given list or string, ranging between the first position (inclusive) and the second position (exclusive). 
-- 0-indexed
sublist l x y = take (y-x) . drop x $ l
{-# INLINE substring #-}
-- | Reports just a section of the given list or string, ranging between the first position (inclusive) and the second position (exclusive). 
substring = sublist

-- stringp
-- no dynamic typing

{-# INLINE read_from_string #-}
-- | Interprets the given string as if it had been typed in the Command Center, and reports the resulting value.
read_from_string :: Read a => String -> a
read_from_string = read

-- word
-- no dynamic typing

-- Mathematics Operators
------------
-- plus
-- minus
-- div_
-- pow
-- lt
-- gt
-- equalp = ==
-- notequalp = /=
-- le
-- ge

{-# INLINE abs_ #-}
-- | Reports the absolute value of number. 
abs_ :: (Monad m, Num a) => a -> C m a
abs_ = return . abs

{-# INLINE e #-}
-- | Mathematical Constant
e = exp 1

{-# INLINE exp_ #-}
-- | Reports the value of e raised to the number power. 
exp_ = exp

{-# INLINE pi_ #-}
-- | Mathematical Constant
pi_ = pi

-- | Reports the cosine of the given angle. Assumes the angle is given in degrees. 
cos_ = cos . toRadians

-- | Reports the sine of the given angle. Assumes the angle is given in degrees. 
sin_ = sin . toRadians

-- | Reports the tangent of the given angle. 
tan_ = tan . toRadians

-- | Internal
toRadians deg = deg * pi / 180

-- | Internal
toDegrees rad = rad * 180 / pi

-- | Reports number1 modulo number2
x `mod_` y | x == 0 = 0
           | otherwise =  fromIntegral (x' `mod` y) + (x - fromIntegral x')
           where x' = floor x

-- | Reports the arc cosine (inverse cosine) of the given number. 
acos_ = toDegrees . acos

-- | Reports the arc sine (inverse sine) of the given number. 
asin_ = toDegrees . asin

-- | Reports the arc tangent (inverse tangent) of the given number. 
atan_ x y = toDegrees $ atan2 (toRadians x) (toRadians y)

{-# INLINE floor_ #-}
-- | Reports the largest integer less than or equal to number. 
floor_ = floor
{-# INLINE ceiling_ #-}
-- | Reports the smallest integer greater than or equal to number. 
ceiling_ = ceiling

{-# INLINE int #-}
-- | Reports the integer part of number -- any fractional part is discarded. 
int n = truncate n

-- numberp 
-- no dynamic typing

{-# INLINE ln #-}
-- | Reports the natural logarithm of number, that is, the logarithm to the base e (2.71828...). 
ln n = log n

{-# INLINE log_ #-}
-- | Reports the logarithm of number in base base. 
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
mean l = let (t,n) = foldl' (\(b,c) a -> (a+b,c+1)) (0,0) l in realToFrac(t)/realToFrac(n)

-- | Reports the statistical median of the numeric items of the given list. Ignores non-numeric items.
median l = let (d, m) = (length l) `divMod` 2
           in case m of
                1 -> l !! d
                0 -> (l !! d + l !! (d-1)) / 2

-- | Reports a list of the most common item or items in list. 
-- | todo
modes = undefined

{-# INLINE remainder #-}
-- | Reports the remainder when number1 is divided by number2. 
remainder = rem

{-# INLINE round_ #-}
-- | Reports the integer nearest to number. 
round_ = round

{-# INLINE sqrt_ #-}
-- | Reports the square root of number. 
sqrt_ = sqrt

-- | Reports the sample variance of a list of numbers. Ignores other types of items. 
-- | todo
variance = undefined

-- | Reports the sample standard deviation of a list of numbers. Ignores other types of items. 
-- | todo
standard_deviation l = undefined

-- | Computes the difference between the given headings, that is, the number of degrees in the smallest angle by which heading2 could be rotated to produce heading1. 
-- | todo 
subtract_headings :: (Monad m) => Double -> Double -> C m Double
subtract_headings h1 h2 = let 
    h1' = if (h1 < 0 || h1 >= 360)
          then (h1 `mod_` 360 + 360) `mod_` 360
          else h1
    h2' = if (h2 < 0 || h2 >= 360)
          then (h2 `mod_` 360 + 360) `mod_` 360
          else h2
    diff = h1' - h2'
                           in return $
                             if (diff > -180 && diff <= 180)
                             then diff
                             else if (diff > 0)
                                  then diff - 360
                                  else diff + 360

-- let r1 = h2 - h1 `mod_` 180
                          --     r2 = h1 - h2 `mod_` 180
                          -- in return $
                          --   if abs r1 < abs r2
                          --   then if h2 > 180 then -r1 else r1
                          --   else if h2 > 180 then -r2 else r2


{-# INLINE sum_ #-}
-- | Reports the sum of the items in the list. 
sum_ = sum

-- | The link makes itself invisible. 
hide_link :: CSTM ()
hide_link = do
  (_, _, a, _, _,_) <- ask
  case a of
    LinkRef _ (MkLink {lhiddenp_ = h}) -> lift $ writeTVar h True
    _ -> throw $ ContextException "link" a

-- | The turtle becomes visible again. 
show_link :: CSTM ()
show_link = do
  (_, _, a, _, _,_) <- ask
  case a of
    LinkRef _ (MkLink {lhiddenp_ = h}) -> lift $ writeTVar h False
    _ -> throw $ ContextException "link" a


-- | Reports the distance between the endpoints of the link. 
link_length :: CSTM Double
link_length = do
  (_, tw, a, _, _,_) <- ask
  case a of
    LinkRef (f,t) _ -> do
                [TurtleRef _ (MkTurtle {xcor_ = fx, ycor_ = fy})] <- turtle f
                [TurtleRef _ (MkTurtle {xcor_ = tx, ycor_ = ty})] <- turtle t
                x <- lift $ readTVar fx
                y <- lift $ readTVar fy
                x' <- lift $ readTVar tx
                y' <- lift $ readTVar ty
                return $ sqrt ((delta x x' (fromIntegral $ max_pxcor_ conf)) ^ 2 + (delta y y' (fromIntegral $ max_pycor_ conf) ^ 2))
    _ -> throw $ ContextException "link" a

-- | Given the who numbers of the endpoints, reports the link connecting the turtles. If there is no such link reports nobody. To refer to breeded links you must use the singular breed form with the endpoints. 
link :: Int -> Int -> CSTM [AgentRef]
link f t = do
  (_, tw,_, _, _,_) <- ask
  (MkWorld _ _ ls) <- lift $ readTVar tw
  return $ [maybe Nobody (LinkRef (f,t)) $ M.lookup (f,t) ls]

-- | Reports the agentset consisting of all links. 
links :: CSTM [AgentRef]
links = do
  (_,tw,_, _, _,_) <- ask
  (MkWorld _ _ ls) <- lift $ readTVar tw
  return $ nubBy checkForUndirected $ M.foldrWithKey (\ k x ks -> LinkRef k x: ks) [] ls
      where
        checkForUndirected (LinkRef (e1,e2) (MkLink {directed_ = False})) (LinkRef (e1',e2') (MkLink {directed_ = False})) = e1 == e2' && e1' == e2
        checkForUndirected _ _ = False


-- | Report the undirected link between turtle and the caller. If no link exists then it reports nobody. 
link_with :: [AgentRef] -> CSTM [AgentRef]
link_with [TurtleRef x _] = do
  (_, _, TurtleRef y _, _, _,_) <- ask
  lxy <- link x y
  lyx <- link y x
  return $ case (lxy,lyx) of
             ([Nobody], [Nobody]) -> [Nobody]
             ([Nobody], _) -> error "directed link"
             ([LinkRef _ _], [LinkRef _ _]) -> lxy -- return arbitrary 1 of the two link positions
             (_, [Nobody]) -> error "directed link"
  
-- | Report the directed link from turtle to the caller. If no link exists then it reports nobody. 
in_link_from :: [AgentRef] -> CSTM [AgentRef]
in_link_from [TurtleRef x _] = do
  (_, _, a, _, _,_) <- ask
  case a of
    TurtleRef y _ -> do
           lxy <- link x y
           lyx <- link y x
           return $ case (lxy,lyx) of
                      ([Nobody], _) -> [Nobody]
                      (_, [Nobody]) -> lxy
                      ([LinkRef _ _], [LinkRef _ _]) -> error "undirected link"
    _ -> throw $ ContextException "turtle" a


-- | Reports the directed link from the caller to turtle. If no link exists then it reports nobody. 
out_link_to :: [AgentRef] -> CSTM [AgentRef]
out_link_to [TurtleRef x _] = do
  (_, _, a, _, _,_) <- ask
  case a of
    TurtleRef y _ -> do
           lxy <- link x y
           lyx <- link y x
           return $ case (lyx,lxy) of
                      ([Nobody], _) -> [Nobody]
                      (_, [Nobody]) -> lyx
                      ([LinkRef _ _], [LinkRef _ _]) -> error "undirected link"
    _ -> throw $ ContextException "turtle" a


-- | Reports an agentset of all undirected links connected to the caller. 
my_links :: CSTM [AgentRef]
my_links = do
  (_, tw,a, _, _,_) <- ask 
  case a of
     TurtleRef x _ -> do
             (MkWorld _ _ ls) <- lift $ readTVar tw
             return $ map (uncurry LinkRef) $ M.assocs $ M.intersection (M.filterWithKey (\ (f,_) _ -> f == x) ls) (M.filterWithKey (\ (_,t) _ -> t == x) ls)
     _ -> throw $ ContextException "turtle" a

-- | Reports an agentset of all the directed links going out from the caller to other nodes. 
my_out_links :: CSTM [AgentRef]
my_out_links = do
  (_, tw, a, _, _,_) <- ask 
  case a of
    TurtleRef x _ -> do
                 (MkWorld _ _ ls) <- lift $ readTVar tw
                 return $ map (uncurry LinkRef) $ M.assocs $ M.filterWithKey (\ (f,_) _ -> f == x) ls
    _ -> throw $ ContextException "turtle" a

-- |  Reports an agentset of all the directed links coming in from other nodes to the caller. 
my_in_links :: CSTM [AgentRef]
my_in_links = do
  (_, tw, a, _, _,_) <- ask 
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
  (_, _, a, _, _,_) <- ask
  case a of
    LinkRef _ (MkLink {tie_mode = t}) -> lift $ writeTVar t Fixed
    _ -> throw $ ContextException "link" a

-- | Unties end2 from end1 (sets tie-mode to "none") if they were previously tied together. If the link is an undirected link, then it will untie end1 from end2 as well. It does not remove the link between the two turtles. 
untie :: CSTM ()
untie = do
  (_, _, a, _, _,_) <- ask
  case a of
    LinkRef _ (MkLink {tie_mode = t}) -> lift $ writeTVar t None
    _ -> throw $ ContextException "link" a

-- | Reports an agentset containing all of the links anywhere in any of the inputs.
-- | NB: HLogo no support for nested turtle_set concatenation/flattening
link_set :: Monad m => [C m [AgentRef]] -> C m [AgentRef]
link_set ts = sequence ts >>= return . foldr (\ x acc -> 
                                                if x == Nobody -- filter Nobody
                                                then acc
                                                else case x of -- type check
                                                       LinkRef _ _ -> if x `elem` acc -- nub
                                                                      then acc
                                                                      else (x:acc)
                                                       _ -> throw $ TypeException "link" x
                                               ) [] . concat

end1 :: CSTM [AgentRef]
end1 = do
  (_,_,a,_,_,_) <- ask
  case a of
    LinkRef (e1, e2) _ -> turtle e1
    _ -> throw $ ContextException "link" a

end2 :: CSTM [AgentRef]
end2 = do
  (_,_,a,_,_,_) <- ask
  case a of
    LinkRef (e1, e2) _ -> turtle e2
    _ -> throw $ ContextException "link" a


-- | lifting STM to IO, a wrapper to atomically
atomic :: CSTM a -> CIO a
atomic = mapReaderT atomically

-- | The specified agent or agentset runs the given commands. 
ask_ :: CIO a -> [AgentRef] -> CIO ()
ask_ f as = do
 (gs, tw, s, p, g,_) <- ask
 case s of
   Nobody -> throw $ ContextException "agent" Nobody
   _ -> return ()
 if as == [Nobody] then throw $ TypeException "agentset" Nobody else return ()
 let [as1,as2,as3,as4] = split 4 as
 (_, wait1) <- lift $ Thread.forkIO (sequence_ [runReaderT f (gs, tw, a, p, g, s) | a <- as1])
 (_, wait2) <- lift $ Thread.forkIO (sequence_ [runReaderT f (gs, tw, a, p, g, s) | a <- as2])
 (_, wait3) <- lift $ Thread.forkIO (sequence_ [runReaderT f (gs, tw, a, p, g, s) | a <- as3])
 (_, wait4) <- lift $ Thread.forkIO (sequence_ [runReaderT f (gs, tw, a, p, g, s) | a <- as4])
 lift wait1
 lift wait2
 lift wait3
 lift wait4
 return ()
 -- tg <- lift ThreadGroup.new
 -- lift . sequence_ $ [ThreadGroup.forkIO tg (runReaderT f (gs, tw, a, p, g, s)) | a <- as]
 -- lift $ ThreadGroup.wait tg

-- | Internal
split :: Int -> [a] -> [[a]]
split n l = let (d,m) = length l `divMod` n
                split' 0 m l = []
                split' x 0 l = let (t, r) = splitAt d l
                               in t : split' (x-1) 0 r
                split' x m l = let (t, r) = splitAt (d+1) l
                               in t : split' (x-1) (m-1) r
            in split' n m l

-- | For an agent, reports the value of the reporter for that agent (turtle or patch). 
--  For an agentset, reports a list that contains the value of the reporter for each agent in the agentset (in random order). 
of_ :: CIO a -> [AgentRef] -> CIO [a]
of_ f as = do
  (gs, tw, s, p, g,_) <- ask
  case s of
    Nobody -> throw $ ContextException "agent" Nobody
    _ -> return ()
  if as == [Nobody] then throw $ TypeException "agentset" Nobody else return ()
  let [as1,as2,as3,as4] = split 4 as
  (_, wait1) <- lift $ Thread.forkIO (sequence [runReaderT f (gs, tw, a, p, g, s) | a <- as1])
  (_, wait2) <- lift $ Thread.forkIO (sequence [runReaderT f (gs, tw, a, p, g, s) | a <- as2])
  (_, wait3) <- lift $ Thread.forkIO (sequence [runReaderT f (gs, tw, a, p, g, s) | a <- as3])
  (_, wait4) <- lift $ Thread.forkIO (sequence [runReaderT f (gs, tw, a, p, g, s) | a <- as4])
  r1 <- lift $ Thread.result =<< wait1
  r2 <- lift $ Thread.result =<< wait2
  r3 <- lift $ Thread.result =<< wait3
  r4 <- lift $ Thread.result =<< wait4
  return $ r1 ++ r2 ++ r3 ++ r4

  -- xs <- lift . sequence $ [Thread.forkIO (runReaderT f (gs, tw, a, p, g, s)) | a <- as]
  -- lift $ mapM (\(_, wait) -> wait >>= Thread.result ) xs

-- | Takes two inputs: an agentset and a boolean reporter. Reports a new agentset containing only those agents that reported true 
-- in other words, the agents satisfying the given condition. 
with :: CIO Bool -> [AgentRef] -> CIO [AgentRef]
with f as = do
  res <- f `of_` as
  return $ foldr (\ (a, r) l -> if r then (a:l) else l) [] (zip as res)

-- |  Runs the list of commands forever, or until the current procedure exits through use of the stop command or the report command. 
-- NB: Report command will not stop it in HLogo, only the stop primitive. 
-- | todo: use MaybeT or ErrorT
-- This command is only run in IO, bcs the command has been implemented
-- using exceptions and exceptions don't work the same in STM. Also
-- it avoids common over-logging that can happen in STM.
loop :: CIO a -> CIO ()
loop c = catchIO (forever c) (\ e -> let _ = e :: StopException in return ())

-- | This agent exits immediately from the enclosing procedure, ask, or ask-like construct (e.g. crt, hatch, sprout). Only the current procedure stops, not all execution for the agent. 
stop = throw StopException

-- | If reporter reports false, exit the loop. Otherwise run commands and repeat. 
-- | todo: use MaybeT or ErrorT
-- This command is only run in IO, bcs the command has been implemented
-- using exceptions and exceptions don't work the same in STM. Also
-- it avoids common over-logging that can happen in STM.
while :: CIO Bool -> CIO a -> CIO ()
while r c = r >>= (\ res -> if res 
                           then catchIO (c >> while r c) (\ e -> let _ = e :: StopException in return ())
                           else return ())


-- Type-safe Casts

is_turtlep :: (Monad m, Typeable a) => a -> C m Bool
is_turtlep t = return $ maybe False (\ t -> case t of
                                    [TurtleRef _ _] -> True
                                    _ -> False)
                                         (cast t :: Maybe [AgentRef])

is_patchp :: (Monad m, Typeable a) => a -> C m Bool
is_patchp t = return $ maybe False (\ t -> case t of
                                    [PatchRef _ _] -> True
                                    _ -> False)
                                         (cast t :: Maybe [AgentRef])

is_agentp :: (Monad m, Typeable a) => a -> C m Bool
is_agentp t = return $ maybe False (\ t -> case t of -- check for a single agent
                                    [_] -> True
                                    _ -> False) (cast t :: Maybe [AgentRef])

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

--is_listp :: (Monad m, Typeable a) => a -> C m Bool
--is_listp :: (Typeable a, Typeable t) => t -> [a]
--is_listp l =  (cast l :: Typeable a => Maybe [a])
-- | todo: not currently implemented
is_listp = undefined

is_stringp :: (Monad m, Typeable a) => a -> C m Bool
is_stringp s = return $ maybe False (const True) (cast s :: Maybe String)

is_numberp :: (Monad m, Typeable a) => a -> C m Bool
is_numberp n = return $ is_intp n || is_integerp n || is_floatp n || is_doublep n
               where
                 is_intp n = maybe False (const True) (cast n :: Maybe Int)
                 is_integerp n = maybe False (const True) (cast n :: Maybe Integer)
                 is_floatp n = maybe False (const True) (cast n :: Maybe Float)
                 is_doublep n = maybe False (const True) (cast n :: Maybe Double)

is_linkp :: (Monad m, Typeable a) => a -> C m Bool
is_linkp l = return $ maybe False (\ l -> case l of
                                    [LinkRef _ _] -> True
                                    _ -> False)
                                         (cast l :: Maybe [AgentRef])

is_directed_linkp :: (Monad m, Typeable a) => a -> C m Bool
is_directed_linkp l = return $ maybe False (\ l -> case l of
                                    [LinkRef _ (MkLink {directed_ = d})] -> d
                                    _ -> False)
                                         (cast l :: Maybe [AgentRef])

is_undirected_linkp :: (Monad m, Typeable a) => a -> C m Bool
is_undirected_linkp = liftM not . is_directed_linkp



-- | Checks only the 1st element
-- | todo: would require a datatype distinction between agentrefs
is_link_setp (l:_) = is_linkp l


-- | This turtle creates number new turtles. Each new turtle inherits of all its variables, including its location, from its parent. (Exceptions: each new turtle will have a new who number)
hatch :: Int -> CSTM [AgentRef]
hatch n = do
  (gs, tw, a, _, _,_) <- ask
  case a of
    TurtleRef _ mt -> do
            let who = gs ! 0
            let arr = tvars_ mt
            let b = bounds arr
            let newArray = return . listArray b =<< sequence [lift (newTVar =<< readTVar (arr ! i)) | i <- [fst b.. snd b]]
            let newTurtles w n = return . IM.fromAscList =<< sequence [do
                                                                        a <- newArray
                                                                        return (i, mt { who_ = i, tvars_ = a}) | i <- [w..w+n-1]]
            let addTurtles ts' (MkWorld ps ts ls)  = MkWorld ps (ts `IM.union` ts') ls
            oldWho <- lift $ liftM round $ readTVar who
            lift $ modifyTVar' who (\ ow -> fromIntegral n + ow)
            ns <- newTurtles oldWho n
            lift $ modifyTVar' tw (addTurtles ns) 
            return $ map (uncurry TurtleRef) $ IM.toList ns -- todo: can be optimized

    _ -> throw $ ContextException "turtle" a

-- | The turtle sets its x and y coordinates to be the same as the given agent's.
-- (If that agent is a patch, the effect is to move the turtle to the center of that patch.) 
move_to :: [AgentRef] -> CSTM ()
move_to a = do
  (_,_,s,_,_,_) <- ask
  case s of
    TurtleRef _ (MkTurtle {xcor_ = tx, ycor_ = ty}) -> do
              case a of
                [PatchRef (px, py) _] -> lift $ writeTVar tx (fromIntegral px) >> writeTVar ty (fromIntegral py)
                [TurtleRef _ (MkTurtle {xcor_ = tx', ycor_ = ty'})] -> lift $ do
                                                                      x' <- readTVar tx'
                                                                      y' <- readTVar ty'
                                                                      writeTVar tx x'
                                                                      writeTVar ty y'
                _ -> throw $ TypeException "turtle or patch" (head a)
    _ -> throw $ ContextException "turtle" s

-- Unsafe
--

unsafe_turtles_here :: CIO [AgentRef]
unsafe_turtles_here = do
  (_,_,s,_,_,_) <- ask
  h <- case s of
        TurtleRef _ _ -> unsafe_patch_here
        PatchRef _ _ -> return [s]
        _ -> throw $ ContextException "turtle or patch" s
  ts <- unsafe_turtles
  with (return . (== h) =<< unsafe_patch_here) ts


unsafe_turtles_at :: Double -> Double -> CIO [AgentRef]
unsafe_turtles_at x y = do
  (_, _, a, _, _,_) <- ask
  p <- unsafe_patch_at x y
  with (return . (== [a])  =<< unsafe_patch_here) =<< unsafe_turtles


unsafe_patch_at :: Double -> Double -> CIO [AgentRef]
unsafe_patch_at x y = do
  (_, _, a, _, _,_) <- ask
  case a of
    PatchRef (px, py) _ -> unsafe_patch (fromIntegral px) (fromIntegral py)
    TurtleRef _ _ -> do
                 [PatchRef (px, py) _] <- unsafe_patch_here
                 unsafe_patch (fromIntegral px + x) (fromIntegral py +y)
    _ -> throw $ ContextException "turtle or patch" a
                 

unsafe_patches :: CIO [AgentRef]
unsafe_patches = do
  (_,tw,_, _, _,_) <- ask
  (MkWorld ps _ _) <- lift $ readTVarIO tw
  return $ M.foldrWithKey (\ k x ks -> PatchRef k x: ks) [] ps


unsafe_patch :: Double -> Double -> CIO [AgentRef]
unsafe_patch x y = do
  (_, tw,_, _, _,_) <- ask
  (MkWorld ps _ _) <- lift $ readTVarIO tw
  return $ if (not (horizontal_wrap_ conf) && (x' > max_pxcor_ conf || x' < min_pxcor_ conf)) || (not (vertical_wrap_ conf) && (y' > max_pycor_ conf || y' < min_pycor_ conf))
           then [Nobody]
           else
               [PatchRef (x'', y'') (ps M.! (x'', y''))]
         where
           x' = round x
           y' = round y
           (x'',y'') = normalize x' y'
           normalize :: Int -> Int -> (Int, Int)
           normalize x y = (
                            ((x + max_pxcor_ conf) `mod` (max_pxcor_ conf*2+1)) - max_pxcor_ conf,
                            ((y + max_pycor_ conf) `mod` (max_pycor_ conf*2+1)) - max_pycor_ conf
                           )

unsafe_link :: Int -> Int -> CIO [AgentRef]
unsafe_link x y = do
  (_, tw,_, _, _,_) <- ask
  (MkWorld _ _ ls) <- lift $ readTVarIO tw
  return $ [LinkRef (x,y) (ls M.! (x,y))]



unsafe_turtles :: CIO [AgentRef]
unsafe_turtles = do
  (_,tw,_, _, _,_) <- ask
  (MkWorld _ ts _) <- lift $ readTVarIO tw
  return $ IM.foldrWithKey (\ k x ks -> TurtleRef k x: ks) [] ts

unsafe_links :: CIO [AgentRef]
unsafe_links = do
  (_,tw,_, _, _,_) <- ask
  (MkWorld _ _ ls) <- lift $ readTVarIO tw
  return $ nubBy checkForUndirected $ M.foldrWithKey (\ k x ks -> LinkRef k x: ks) [] ls
      where
        checkForUndirected (LinkRef (e1,e2) (MkLink {directed_ = False})) (LinkRef (e1',e2') (MkLink {directed_ = False})) = e1 == e2' && e1' == e2
        checkForUndirected _ _ = False

unsafe_patch_here :: CIO [AgentRef]
unsafe_patch_here = do
  (_,_,a, _, _,_) <- ask
  case a of
    TurtleRef _ (MkTurtle {xcor_ = x, ycor_ = y}) -> do
                      x' <- lift $ readTVarIO x
                      y' <- lift $ readTVarIO y
                      unsafe_patch x' y'
    _ -> throw $ ContextException "turtle" a

unsafe_turtle :: Int -> CIO [AgentRef]
unsafe_turtle n = do
  (_, tw,_, _, _,_) <- ask
  (MkWorld _ ts _) <- lift $ readTVarIO tw
  return $ [TurtleRef n (ts IM.! n)]


unsafe_can_movep :: Double -> CIO Bool
unsafe_can_movep n = unsafe_patch_ahead n >>= \ p -> return (p /= [Nobody])

unsafe_patch_ahead ::Double -> CIO [AgentRef]
unsafe_patch_ahead n = do
  x <- unsafe_xcor 
  y <- unsafe_ycor
  dx_ <- unsafe_dx
  dy_ <- unsafe_dy
  let mx = fromIntegral $ max_pxcor_ conf
  let my = fromIntegral $ max_pycor_ conf
  let px_new = fromIntegral (round x) + if horizontal_wrap_ conf
                                        then (dx_*n + mx) `mod_` (truncate mx * 2 + 1) - mx
                                        else dx_*n

  let py_new = fromIntegral (round y) + if vertical_wrap_ conf
                                        then (dy_*n + my) `mod_` (truncate my * 2 + 1) - my
                                        else  dy_*n
  unsafe_patch px_new py_new

unsafe_dx :: CIO Double
unsafe_dx = liftM sin_ unsafe_heading

unsafe_dy :: CIO Double
unsafe_dy = liftM cos_ unsafe_heading

unsafe_heading :: CIO Double
unsafe_heading = do
  (_,_,a, _, _,_) <- ask
  case a of
    TurtleRef _ (MkTurtle {heading_ = h}) -> lift $ readTVarIO h
    _ -> throw (ContextException "turtle" a)

unsafe_xcor :: CIO Double
unsafe_xcor = do
  (_,_,a, _, _,_) <- ask
  case a of
    TurtleRef _ (MkTurtle {xcor_ = x}) -> lift $ readTVarIO x
    _ -> throw $ ContextException "turtle" a

unsafe_ycor :: CIO Double
unsafe_ycor = do
  (_,_,a, _, _,_) <- ask
  case a of
    TurtleRef _ (MkTurtle {ycor_ = y}) -> lift $ readTVarIO y
    _ -> throw $ ContextException "turtle" a

unsafe_color :: CIO Double
unsafe_color = do
  (_,_,a, _, _,_) <- ask
  case a of
    TurtleRef _ (MkTurtle {color_ = c}) -> lift $ readTVarIO c
    _ -> throw $ ContextException "turtle" a


unsafe_random_xcor :: CIO Double
unsafe_random_xcor = lift $ getStdRandom $ randomR ((fromIntegral $ min_pxcor_ conf),(fromIntegral $ max_pxcor_ conf))

unsafe_random_ycor :: CIO Double
unsafe_random_ycor = lift $ getStdRandom $ randomR ((fromIntegral $ min_pycor_ conf),(fromIntegral $ max_pycor_ conf))

unsafe_random_pxcor :: CIO Int
unsafe_random_pxcor = lift $ getStdRandom $ randomR (min_pxcor_ conf, max_pxcor_ conf)

unsafe_random_pycor :: CIO Int
unsafe_random_pycor = lift $ getStdRandom $ randomR (min_pycor_ conf, max_pycor_ conf)

unsafe_random               :: (Random a , Eq a, Ord a, Num a) => a -> CIO a
unsafe_random x | x == 0 = return 0
         | x < 0 = lift $ getStdRandom $ randomR (x,0)
         | x > 0 = lift $ getStdRandom $ randomR (0,x)

unsafe_random_float :: Double -> CIO Double
unsafe_random_float x | x == 0 = return 0
               | x < 0 = lift $ getStdRandom $ randomR (x,0)
               | x > 0 = lift $ getStdRandom $ randomR (0,x)

-- | Internal
unsafe_random_primary_color :: CIO Double
unsafe_random_primary_color = do
  i <- lift $ randomRIO (0,13)
  return $ primary_colors !! i


-- | Reports a number suitable for seeding the random number generator. 
-- | todo
unsafe_new_seed = undefined

-- | Sets the seed of the pseudo-random number generator to the integer part of number.
unsafe_random_seed n = setStdGen $ mkStdGen n

-- | random-exponential reports an exponentially distributed random floating point number. 
-- | todo
unsafe_random_exponential m = undefined

-- | random-gamma reports a gamma-distributed random floating point number as controlled by the floating point alpha and lambda parameters. 
-- | todo
unsafe_random_gamma a l = undefined

-- | random-normal reports a normally distributed random floating point number. 
-- | todo
unsafe_random_normal m s = undefined

-- | random-poisson reports a Poisson-distributed random integer. 
-- | todo
unsafe_random_poisson m = undefined


-- | Reports an agentset containing all the turtles that are on the given patch or patches, or standing on the same patch as the given turtle or turtles. 
turtles_on :: [AgentRef] -> CIO [AgentRef]
turtles_on [] = return []
turtles_on ps@(PatchRef _ _ : _) = do
  with (liftM (flip elem ps . head) unsafe_patch_here) =<< unsafe_turtles
turtles_on ts@(TurtleRef _ _ : _) = do
  turtles_on =<< of_ (liftM head unsafe_patch_here) ts
turtles_on (a:_) = throw $ ContextException "turtle or patch agentset" a


unsafe_distance :: [AgentRef] -> CIO Double
unsafe_distance [PatchRef (x,y) _] = do
  unsafe_distancexy (fromIntegral x) (fromIntegral y)
unsafe_distance [TurtleRef _ (MkTurtle {xcor_ = tx, ycor_ = ty})] = do
  x <- lift $ readTVarIO tx
  y <- lift $ readTVarIO ty
  unsafe_distancexy x y
unsafe_distance (a:_) = throw $ ContextException "single turtle or patch" a

unsafe_distancexy :: Double -> Double -> CIO Double
unsafe_distancexy x' y' = do
  (_,_,a,_,_,_) <- ask
  (x,y) <- case a of
            PatchRef (x,y) _ -> return (fromIntegral x, fromIntegral y)
            TurtleRef _ (MkTurtle {xcor_ = tx, ycor_ = ty}) -> liftM2 (,) (lift $ readTVarIO tx) (lift $ readTVarIO ty)
            _ -> throw $ ContextException "turtle or patch" a
  return $ sqrt ((delta x x' (fromIntegral $ max_pxcor_ conf)) ^ 2 + (delta y y' (fromIntegral $ max_pycor_ conf) ^ 2))
      where
        delta a1 a2 aboundary = min (abs (a2 - a1)) (abs (a2 + a1) + 1)

-- | todo
unsafe_downhill = undefined

-- | todo
unsafe_downhill4 = undefined

unsafe_towards :: [AgentRef] -> CIO Double
unsafe_towards a = do
  (_, _, s, _, _, _) <- ask
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
  let dx = x2 - x1
  let dy = y2 - y1
  return $ if (dx == 0)
            then
                if dy > 0 
                then 0 
                else 180
            else
                if (dy == 0)
                then if dx > 0 
                     then 90 
                     else 270
                else (270 + toDegrees (pi + atan2 (-dy) dx)) `mod_` 360


-- | todo
unsafe_towardsxy = undefined

-- |  Reports a subset of the given agentset that includes only the agents on the patches the given distances away from this agent. The distances are specified as a list of two-item lists, where the two items are the x and y offsets.
-- If the caller is the observer, then the points are measured relative to the origin, in other words, the points are taken as absolute patch coordinates.
-- If the caller is a turtle, the points are measured relative to the turtle's exact location, and not from the center of the patch under the turtle. 
-- | todo
at_points :: [AgentRef] -> [(Double, Double)] -> CSTM [AgentRef]
at_points (a:as) ds = do
  (_,_,s,_,_,_) <- ask
  (x,y) <- case s of
            ObserverRef -> return (0,0)
            PatchRef (px, py) _ -> return (fromIntegral px, fromIntegral py)
            TurtleRef _ (MkTurtle {xcor_ = tx, ycor_ = ty}) -> lift $ liftM2 (,) (readTVar tx) (readTVar ty)
  return []

unsafe_in_radius :: [AgentRef] -> Double -> CIO [AgentRef]
unsafe_in_radius as n = do
  (_,_,a,_,_,_) <- ask
  (x, y) <- case a of
    PatchRef (x,y) _ -> return $ (fromIntegral x, fromIntegral y)
    TurtleRef _ (MkTurtle {xcor_ = tx, ycor_ = ty}) -> liftM2 (,) (lift $ readTVarIO tx) (lift $ readTVarIO ty)
    _ -> throw $ ContextException "turtle or patch" a
  with (unsafe_distancexy x y >>= \ d -> return $ d <= n) as

unsafe_in_cone = undefined

-- | Runs the given commands only if it's been more than number seconds since the last time this agent ran them in this context. Otherwise, the commands are skipped. 
-- | NB: Works differently than NetLogo, in that only the calling thread is suspended, not the whole simulation
unsafe_every :: Double -> CIO a -> CIO ()
unsafe_every n a = a >> unsafe_wait n

-- | Wait the given number of seconds. (This needn't be an integer; you can specify fractions of seconds.) Note that you can't expect complete precision; the agent will never wait less than the given amount, but might wait slightly more. 
-- | NB: Works differently than NetLogo, in that only the calling thread is suspended, not the whole simulation
unsafe_wait n = lift $ threadDelay (round $ n * 1000000)

unsafe_ticks :: CIO Double
unsafe_ticks = do
  (gs, _, _, _, _,_) <- ask
  lift $ readTVarIO (gs ! 1)

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


-- | todo optimize with arrays <http://www.haskell.org/haskellwiki/Random_shuffle>
unsafe_shuffle :: Eq a => [a] -> CIO [a]
unsafe_shuffle [] = return []
unsafe_shuffle l = shuffle' l (length l) where
    shuffle [x] 1 = return [x]
    shuffle' l i = do
      [x] <- unsafe_one_of l
      xs <- shuffle' (delete x l) (i-1)
      return $ x:xs


unsafe_show_ :: Show a => a -> CIO ()
unsafe_show_ a = do
  (_,_, r, _, _,_) <- ask
  lift $ putStrLn $ (case r of
                           ObserverRef -> "observer: "
                           PatchRef (x,y) _ -> "(patch " ++ show x ++ " " ++ show y ++ "): "
                           LinkRef (x,y) _ -> "(link " ++ show x ++ " " ++ show y ++ "): "
                           TurtleRef i _ -> "(turtle " ++ show i ++ "): ")   ++ show a


unsafe_print_ :: Show a => a -> CIO ()
unsafe_print_ a = do
  lift $ putStrLn $ show a



with_breed :: (String -> String) -> CSTM ()
with_breed f = do
  (_,_,s,_,_,_) <- ask
  case s of
    TurtleRef _ (MkTurtle {breed_ = tb}) -> lift $ modifyTVar' tb f
    --LinkRef _ (MkLink {lbreed_ = tb}) -> lift $ modifyTVar' tb f
    _ -> throw $ ContextException "turtle" s

with_color :: (Double -> Double) -> CSTM ()
with_color f = do
  (_,_,s,_,_,_) <- ask
  case s of
    TurtleRef _ (MkTurtle {color_ = tb}) -> lift $ modifyTVar' tb f
    LinkRef _ (MkLink {lcolor_ = tb}) -> lift $ modifyTVar' tb f
    _ -> throw $ ContextException "turtle or link" s

with_heading :: (Double -> Double) -> CSTM ()
with_heading f = do
  (_,_,s,_,_,_) <- ask
  case s of
    TurtleRef _ (MkTurtle {heading_ = tb}) -> lift $ modifyTVar' tb f
    _ -> throw $ ContextException "turtle" s

with_shape :: (String -> String) -> CSTM ()
with_shape f = do
  (_,_,s,_,_,_) <- ask
  case s of
    TurtleRef _ (MkTurtle {shape_ = tb}) -> lift $ modifyTVar' tb f
    _ -> throw $ ContextException "turtle" s

with_label :: (String -> String) -> CSTM ()
with_label f = do
  (_,_,s,_,_,_) <- ask
  case s of
    TurtleRef _ (MkTurtle {label_ = tb}) -> lift $ modifyTVar' tb f
    LinkRef _ (MkLink {llabel_ = tb}) -> lift $ modifyTVar' tb f
    _ -> throw $ ContextException "turtle or link" s

with_label_color :: (Double -> Double) -> CSTM ()
with_label_color f = do
  (_,_,s,_,_,_) <- ask
  case s of
    TurtleRef _ (MkTurtle {label_color_ = tb}) -> lift $ modifyTVar' tb f
    LinkRef _ (MkLink {llabel_color_ = tb}) -> lift $ modifyTVar' tb f
    _ -> throw $ ContextException "turtle or link" s

with_size :: (Double -> Double) -> CSTM ()
with_size f = do
  (_,_,s,_,_,_) <- ask
  case s of
    TurtleRef _ (MkTurtle {size_ = tb}) -> lift $ modifyTVar' tb f
    _ -> throw $ ContextException "turtle" s

with_pcolor :: (Double -> Double) -> CSTM ()
with_pcolor f = do
  (_,_,s,_,_,_) <- ask
  case s of
    PatchRef _ (MkPatch {pcolor_ = tb}) -> lift $ modifyTVar' tb f
    _ -> throw $ ContextException "patch" s

with_plabel :: (String -> String) -> CSTM ()
with_plabel f = do
  (_,_,s,_,_,_) <- ask
  case s of
    PatchRef _ (MkPatch {plabel_ = tb}) -> lift $ modifyTVar' tb f
    _ -> throw $ ContextException "patch" s

with_plabel_color :: (Double -> Double) -> CSTM ()
with_plabel_color f = do
  (_,_,s,_,_,_) <- ask
  case s of
    PatchRef _ (MkPatch {plabel_color_ = tb}) -> lift $ modifyTVar' tb f
    _ -> throw $ ContextException "patch" s

snapshot :: CIO ()
snapshot = do
  (_, tw, s, _, _, _) <- ask
  case s of
    ObserverRef -> do
             t <- unsafe_ticks
             w <- lift $ readTVarIO tw
             p <- patch_size
             max_x <- max_pxcor
             min_x <- min_pycor
             let sizeSpec = Diag.Width (fromIntegral (p * (max_x + abs min_x + 1)))
             let output = ("snapshot" ++ show (round t) ++ ".eps")
             prs <- unsafe_patches
             diagPatches <- lift $ mapM (\ (PatchRef (px,py) p) -> do 
                                   c <- readTVarIO $ pcolor_ p
                                   let [r,g,b] = extract_rgb c
                                   return (Diag.p2 (fromIntegral px, fromIntegral py), Diag.square 1 Diag.# Diag.fc (sRGB24 r g b) :: Diag.Diagram Postscript Diag.R2)
                                ) prs
             trs <- unsafe_turtles
             diagTurtles <- lift $ mapM (\ (TurtleRef _ t) -> do 
                                          x <- readTVarIO $ xcor_ t
                                          y <- readTVarIO $ ycor_ t
                                          c <- readTVarIO $ color_ t
                                          h <- readTVarIO $ heading_ t
                                          s <- readTVarIO $ size_ t
                                          let [r,g,b] = extract_rgb c
                                          return (Diag.p2 (x, y), Diag.eqTriangle s Diag.# Diag.fc (sRGB24 r g b) Diag.# Diag.scaleX 0.5 Diag.# Diag.rotate (Diag.Deg (-h)) :: Diag.Diagram Postscript Diag.R2)
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
                                step = (fromIntegral (colorTimesTen `rem` 100 - 50)) / 50.48 + 0.012
                            in
                              if step < 0
                              then [truncate(fromIntegral r * step) +r, truncate(fromIntegral g*step)+g, truncate(fromIntegral b*step)+b]
                              else [truncate((255 - fromIntegral r)*step)+r, truncate((255 - fromIntegral g)*step)+g, truncate((255 - fromIntegral b)*step)+b]

