module Framework.Logo.Prim (
                           -- * Agent related
                           self, other, count, distance, unsafe_distance, distancexy, unsafe_distancexy, towards, unsafe_towards, towardsxy, unsafe_towardsxy, in_radius, unsafe_in_radius, in_cone, unsafe_in_cone, unsafe_every, unsafe_wait, is_agentp, is_agentsetp, 

                           -- * Turtle related
                           turtles_here, unsafe_turtles_here, turtles_at, unsafe_turtles_at, unsafe_turtles_on, jump, setxy, forward, fd, back, bk, turtles, unsafe_turtles, turtle, unsafe_turtle, turtle_set, face, xcor, set_xcor, unsafe_xcor, heading, set_heading, unsafe_heading, ycor, set_ycor, unsafe_ycor, who, color, unsafe_color, breed, dx, unsafe_dx, dy, unsafe_dy, home, right, rt, unsafe_right, left, lt, unsafe_left, downhill, unsafe_downhill, downhill4, unsafe_downhill4,  hide_turtle, ht, show_turtle, st, pen_down, pd, pen_up, pu, pen_erase, pe, no_turtles, unsafe_no_turtles, is_turtlep, is_turtle_setp,

                           -- * Patch related
                           patch_at, unsafe_patch_at, patch_here, unsafe_patch_here, patch_ahead, unsafe_patch_ahead, patches, unsafe_patches, patch, unsafe_patch, patch_set, can_movep, unsafe_can_movep, no_patches, unsafe_no_patches, is_patchp, is_patch_setp,

                           -- * Link related
                           hide_link, show_link, is_linkp, is_directed_linkp, is_undirected_linkp, is_link_setp, link_length, link, links, link_with, in_link_from, out_link_to, my_links, my_out_links, my_in_links, no_links, tie, untie, 

                           -- * Random related
                           random_xcor, unsafe_random_xcor, random_ycor, unsafe_random_ycor, random_pxcor, unsafe_random_pxcor, random_pycor, unsafe_random_pycor, random, unsafe_random, random_float, unsafe_random_float, unsafe_new_seed, unsafe_random_seed, unsafe_random_exponential, unsafe_random_gamma, unsafe_random_normal, unsafe_random_poisson,

                           -- * Color
                           primary_colors, black, white, gray, red, orange, brown, yellow, green, lime, turquoise, cyan, sky, blue, violet, magenta, pink,

                           -- * List related
                           sum_, anyp, item, one_of, unsafe_one_of, remove, remove_item, replace_item, shuffle, unsafe_shuffle, sublist, substring, n_of, but_first, but_last, emptyp, first, foreach, fput, last_, length_, list, lput, map_, memberp, position, reduce, remove_duplicates, reverse_, sentence, sort_, sort_by, sort_on, max_, min_,n_values, is_stringp,

                           -- * Math
                           xor, e, exp_, pi_, cos_, sin_, tan_, mod_, acos_, asin_, atan_, int, log_, mean, median, modes, variance, standard_deviation, subtract_headings, abs_, floor_, ceiling_, remainder, round_, sqrt_,  is_numberp,

                           -- * Misc
                           max_pxcor, max_pycor, min_pxcor, min_pycor, world_width, world_height, clear_all, ca, clear_all_plots, clear_drawing, cd, clear_output, clear_turtles, ct, clear_patches, cp, clear_links, clear_ticks, reset_ticks, tick, tick_advance, ticks, unsafe_ticks, histogram, 

                           -- * Input/Output
                           show_, unsafe_show_, print_, unsafe_print_, read_from_string,

                           -- * IO Operations
                           atomic, ask_, of_, with


) where

import Framework.Logo.Base
import Framework.Logo.Conf
import Control.Concurrent.STM
import Control.Monad.Reader
import Control.Concurrent (threadDelay)
import qualified Control.Concurrent.Thread as Thread
import qualified Control.Concurrent.Thread.Group as ThreadGroup
import Data.List
import qualified Data.IntMap as IM
import qualified Data.Map as M
import Data.Array
import Control.Applicative
import System.Random hiding (random)
import Control.Monad (forM_)
import Data.Function
import Data.Maybe (maybe, fromJust)
import Data.Typeable

-- |  Reports this turtle or patch. 
self :: Monad m => C m [AgentRef] -- ^ returns a list (set) of agentrefs to be compatible with the 'turtle-set' function
self = do
  (_, _, a, _, _) <- ask
  return [a]

-- |  Reports an agentset which is the same as the input agentset but omits this agent. 
other :: Monad m => [AgentRef] -> C m [AgentRef]
other as = do
  [s] <- self
  return $ delete s as

-- |  Reports an agentset containing all the turtles on the caller's patch (including the caller itself if it's a turtle). 
turtles_here :: CSTM [AgentRef]
turtles_here = do
  [s] <- self
  [PatchRef (px,py) _] <- patch_here
  ts <- turtles
  filterM (\ (TurtleRef _ (MkTurtle {xcor_ = x, ycor_ = y})) -> do 
             x' <- lift $ readTVar x
             y' <- lift $ readTVar y
             return $ round x' == px && round y' == py
          ) ts

-- |  Reports an agentset containing the turtles on the patch (dx, dy) from the caller. (The result may include the caller itself if the caller is a turtle.) 
turtles_at :: Double -> Double -> CSTM [AgentRef] -- ^ dx -> dy -> CSTM (Set AgentRef)
turtles_at x y = do
  (_, _, a, _, _) <- ask
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
  (_,_, r, p, _) <- ask
  lift $ writeTChan p $ (case r of
                           ObserverRef -> "observer: "
                           PatchRef (x,y) _ -> "(patch " ++ show x ++ " " ++ show y ++ "): "
                           LinkRef (x,y) _ -> "(link " ++ show x ++ " " ++ show y ++ "): "
                           TurtleRef i _ -> "(turtle " ++ show i ++ "): ")   ++ show a

-- | Prints value in the Command Center, followed by a carriage return. 
print_ :: Show a => a -> CSTM ()
print_ a = do
  (_,_, _, p, _) <- ask
  lift $ writeTChan p $ show a
                           

-- | Reports the patch at (dx, dy) from the caller, that is, the patch containing the point dx east and dy patches north of this agent. 
patch_at :: Double -> Double ->  CSTM [AgentRef]
patch_at x y = do
  (_, _, a, _, _) <- ask
  case a of
    PatchRef (px, py) _ -> patch (fromIntegral px) (fromIntegral py)
    TurtleRef _ _ -> do
                 [PatchRef (px, py) _] <- patch_here
                 patch (fromIntegral px + x) (fromIntegral py +y)

-- | patch-here reports the patch under the turtle. 
patch_here :: CSTM [AgentRef]
patch_here = do
  (_,_,TurtleRef _ (MkTurtle {xcor_ = x, ycor_ = y}), _, _) <- ask
  x' <- lift $ readTVar x
  y' <- lift $ readTVar y
  patch x' y'


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
black = 0
-- | NetLogo Constant
white = 9.9
-- | NetLogo Constant
gray = 5
-- | NetLogo Constant
red = 15
-- | NetLogo Constant
orange = 25
-- | NetLogo Constant
brown = 35 
-- | NetLogo Constant
yellow = 45
-- | NetLogo Constant
green = 55
-- | NetLogo Constant
lime = 65
-- | NetLogo Constant
turquoise = 75
-- | NetLogo Constant
cyan = 85
-- | NetLogo Constant
sky = 95
-- | NetLogo Constant
blue = 105
-- | NetLogo Constant
violet = 115
-- | NetLogo Constant
magenta = 125
-- | NetLogo Constant
pink = 135

-- approximate-rgb

-- | Internal
primary_colors :: [Double]
primary_colors = [gray, red, orange, brown, yellow, green, lime, turquoise, cyan, sky, blue, violet, magenta, pink]

-- | Reports the number of agents in the given agentset. 
count :: Monad m => [AgentRef] -> C m Int
count = return . length

-- | Reports true if the given agentset is non-empty, false otherwise. 
anyp :: Monad m => [AgentRef] -> C m Bool
anyp as = return $ not $ null as

-- | Reports the agentset consisting of all patches. 
patches :: CSTM [AgentRef]
patches = do
  (_,tw,_, _, _) <- ask
  (MkWorld ps _ _) <- lift $ readTVar tw
  return $ M.foldrWithKey (\ k x ks -> PatchRef k x: ks) [] ps



-- | Given the x and y coordinates of a point, reports the patch containing that point. 
patch :: Double -> Double -> CSTM [AgentRef]
patch x y = do
  (_, tw,_, _, _) <- ask
  (MkWorld ps _ _) <- lift $ readTVar tw
  return $ if x' > max_pxcor_ conf || x' < min_pxcor_ conf || y' > max_pycor_ conf || y' < min_pycor_ conf
           then [Nobody]
           else
               [PatchRef (x',y') (ps M.! (x',y'))]
         where
           x' = round x
           y' = round y


-- | The turtle moves forward by number units all at once (rather than one step at a time as with the forward command). 
jump :: Double -> CSTM ()
jump n = do
  (_,_, TurtleRef _ (MkTurtle {xcor_ = x, ycor_ = y, heading_ = h}), _, _) <- ask
  h' <- lift $ readTVar h
  lift $ modifyTVar' x (+ (sin_ h' * n)) >>  modifyTVar' y (+ (cos_ h' * n))



-- | The turtle sets its x-coordinate to x and its y-coordinate to y. 
setxy :: Double -> Double -> CSTM ()
setxy x' y' = do
  (_,_, TurtleRef _ (MkTurtle {xcor_ = x, ycor_ = y}), _, _) <- ask
  lift $ writeTVar x x' >> writeTVar y y'


-- | The turtle moves forward by number steps, one step at a time. (If number is negative, the turtle moves backward.) 
forward :: Double -> CSTM ()
forward n | n == 0 = return ()
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

-- | Reports the agentset consisting of all turtles. 
turtles :: CSTM [AgentRef]
turtles = do
  (_,tw,_, _, _) <- ask
  MkWorld _ ts _ <- lift $ readTVar tw
  return $ IM.foldrWithKey (\ k x ks -> TurtleRef k x: ks) [] ts

-- | Reports the turtle with the given who number, or nobody if there is no such turtle. For breeded turtles you may also use the single breed form to refer to them. 
turtle :: Int -> CSTM [AgentRef]
turtle n = do
  (_, tw,_, _, _) <- ask
  (MkWorld _ ts _) <- lift $ readTVar tw
  return [TurtleRef n (ts IM.! n)]


-- | Reports an agentset containing all of the turtles anywhere in any of the inputs.
turtle_set :: Monad m => [C m [AgentRef]] -> C m [AgentRef]
turtle_set ts = sequence ts >>= return . concat

-- | Reports an agentset containing all of the patches anywhere in any of the inputs.
patch_set :: Monad m => [C m [AgentRef]] -> C m [AgentRef]
patch_set = turtle_set

-- | Reports true if this turtle can move distance in the direction it is facing without violating the topology; reports false otherwise. 
can_movep :: Double -> CSTM Bool
can_movep n = patch_ahead n >>= \ p -> return (p /= [Nobody])


-- | This is a built-in turtle variable. It indicates the direction the turtle is facing. 
heading :: CSTM Double
heading = do
  (_,_,TurtleRef _ (MkTurtle {heading_ = h}), _, _) <- ask
  lift $ readTVar h

set_heading :: Double -> CSTM ()
set_heading v = do
  (_,_,TurtleRef _ t, _, _) <- ask
  lift $ writeTVar (heading_ t) v

-- | This is a built-in turtle variable. It holds the current x coordinate of the turtle. 
xcor :: CSTM Double
xcor = do
  (_,_,TurtleRef _ (MkTurtle {xcor_ = x}), _, _) <- ask
  lift $ readTVar x

set_xcor :: Double -> CSTM ()
set_xcor v = do
  (_,_,TurtleRef _ t, _, _) <- ask
  lift $ writeTVar (xcor_ t) v


-- | This is a built-in turtle variable. It holds the current y coordinate of the turtle.
ycor :: CSTM Double
ycor = do
  (_,_,TurtleRef _ (MkTurtle {ycor_ = y}), _, _) <- ask
  lift $ readTVar y

set_ycor :: Double -> CSTM ()
set_ycor v = do
  (_,_,TurtleRef _ t, _, _) <- ask
  lift $ writeTVar (ycor_ t) v

-- | This is a built-in turtle variable. It holds the turtle's "who number" or ID number, an integer greater than or equal to zero. You cannot set this variable; a turtle's who number never changes. 
who :: Monad m => ReaderT Context m Int
who = do
  (_,_,TurtleRef i _, _, _) <- ask
  return i

-- | This is a built-in turtle variable. It holds the turtle's "who number" or ID number, an integer greater than or equal to zero. You cannot set this variable; a turtle's who number never changes. 
color :: CSTM Double
color = do
  (_,_,TurtleRef _ (MkTurtle {color_ = c}), _, _) <- ask
  lift $ readTVar c


breed :: Monad m => ReaderT Context m String
breed = do
  (_,_,TurtleRef _ (MkTurtle {breed_ = b}), _, _) <- ask
  return b

-- | Reports the x-increment (the amount by which the turtle's xcor would change) if the turtle were to take one step forward in its current heading. 
dx :: CSTM Double
dx = liftM sin_ heading

-- | Reports the y-increment (the amount by which the turtle's ycor would change) if the turtle were to take one step forward in its current heading. 
dy :: CSTM Double
dy = liftM cos_ heading

-- | Reports a random floating point number from the allowable range of turtle coordinates along the given axis, x . 
random_xcor :: CSTM Double
random_xcor = do
  (_,_,_,_,ts) <- ask
  s <- lift $ readTVar ts
  let (v, s') = randomR ((fromIntegral $ min_pxcor_ conf),(fromIntegral $ max_pxcor_ conf)) s
  lift $ writeTVar ts s'
  return v

-- | Reports a random floating point number from the allowable range of turtle coordinates along the given axis, y. 
random_ycor :: CSTM Double
random_ycor = do
  (_,_,_,_,ts) <- ask
  s <- lift $ readTVar ts
  let (v, s') = randomR ((fromIntegral $ min_pycor_ conf),(fromIntegral $ max_pycor_ conf)) s
  lift $ writeTVar ts s'
  return v

-- | Reports a random integer ranging from min-pxcor to max-pxcor inclusive. 
random_pxcor :: CSTM Int
random_pxcor = do
  (_,_,_,_,ts) <- ask
  s <- lift $ readTVar ts
  let (v, s') = randomR (min_pxcor_ conf, max_pxcor_ conf) s
  lift $ writeTVar ts s'
  return v

-- | Reports a random integer ranging from min-pycor to max-pycor inclusive. 
random_pycor :: CSTM Int
random_pycor = do
  (_,_,_,_,ts) <- ask
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
  (_,_,_,_,ts) <- ask
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
  (_,_,_,_,ts) <- ask
  s <- lift $ readTVar ts
  let (v, s') = randomR (if x < 0 then (x, 0) else (0,x)) s
  lift $ writeTVar ts s'
  return v

-- | This turtle moves to the origin (0,0). Equivalent to setxy 0 0. 
home :: CSTM ()
home = setxy 0 0

-- | The turtle turns right by number degrees. (If number is negative, it turns left.) 
right :: Double -> CSTM Double
right n = do
  h <- heading
  return $ mod_ (h + n) 360

{-# INLINE rt #-}
-- | alias for 'right'
rt = right

-- | The turtle turns left by number degrees. (If number is negative, it turns right.) 
left :: Double -> CSTM Double
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

delta a1 a2 aboundary =
    min (abs (a2 - a1)) (abs (a2 + a1) + 1)

-- | Reports the distance from this agent to the point (xcor, ycor). 
distancexy :: Double -> Double -> CSTM Double
distancexy x' y' = do
  (_,_,ref,_,_) <- ask
  (x,y) <- case ref of
            PatchRef (x,y) _ -> return (fromIntegral x, fromIntegral y)
            TurtleRef _ (MkTurtle {xcor_ = tx, ycor_ = ty}) -> liftM2 (,) (lift $ readTVar tx) (lift $ readTVar ty)
  return $ sqrt ((delta x x' (fromIntegral $ max_pxcor_ conf)) ^ 2 + (delta y y' (fromIntegral $ max_pycor_ conf) ^ 2))


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
face a = set_heading (towards a)

-- | Reports the heading from this agent to the given agent. 
-- | todo: wrapping
towards = undefined

-- | Reports the heading from the turtle or patch towards the point (x,y). 
-- | todo
towardsxy = undefined


-- | The turtle makes itself invisible. 
hide_turtle :: CSTM ()
hide_turtle = do
  (_,_,TurtleRef _ (MkTurtle {hiddenp_ = th}), _, _) <- ask
  lift $ writeTVar th True

{-# INLINE ht #-}
-- | alias for 'hide_turtle'
ht = hide_turtle

-- | The turtle becomes visible again. 
show_turtle :: CSTM ()
show_turtle = do
  (_,_,TurtleRef _ (MkTurtle {hiddenp_ = th}), _, _) <- ask
  lift $ writeTVar th False

{-# INLINE st #-}
-- | alias for 'show_turtle'
st = show_turtle

-- | The turtle changes modes between drawing lines, removing lines or neither. 
pen_down :: CSTM ()
pen_down = do
  (_,_,TurtleRef _ (MkTurtle {pen_mode_ = tp}), _, _) <- ask
  lift $ writeTVar tp Down

{-# INLINE pd #-}
-- | alias for 'pen_down'
pd = pen_down

-- | The turtle changes modes between drawing lines, removing lines or neither. 
pen_up :: CSTM ()
pen_up = do
  (_,_,TurtleRef _ (MkTurtle {pen_mode_ = tp}), _, _) <- ask
  lift $ writeTVar tp Up

{-# INLINE pu #-}
-- | alias for 'pen_up'
pu = pen_up

pen_erase :: CSTM ()
-- | The turtle changes modes between drawing lines, removing lines or neither. 
pen_erase = do
  (_,_,TurtleRef _ (MkTurtle {pen_mode_ = tp}), _, _) <- ask
  lift $ writeTVar tp Erase

{-# INLINE pe #-}
-- | alias for 'pen_erase'
pe = pen_erase

-- | Reports an agentset that includes only those agents from the original agentset whose distance from the caller is less than or equal to number. (This can include the agent itself.) 
in_radius :: [AgentRef] -> Double -> CSTM [AgentRef]
in_radius as n = do
  (_, _, ref, _, _) <- ask
  (x, y) <- case ref of
             PatchRef (x,y) _ -> return $ (fromIntegral x, fromIntegral y)
             TurtleRef _ (MkTurtle {xcor_ = tx, ycor_ = ty}) -> liftM2 (,) (lift $ readTVar tx) (lift $ readTVar ty)
  filterM (\ (TurtleRef _ (MkTurtle {xcor_ = tx', ycor_ = ty'})) -> do 
             x' <- lift $ readTVar tx'
             y' <- lift $ readTVar ty'
             return $ (sqrt ((delta x x' (fromIntegral $ max_pxcor_ conf)) ^ 2 + (delta y y' (fromIntegral $ max_pycor_ conf) ^ 2))) <= n) as

-- | This reporter lets you give a turtle a "cone of vision" in front of itself. 
in_cone = undefined


-- | Reports an empty turtle agentset. 
no_turtles :: CSTM [AgentRef]
no_turtles = return []

-- | Reports an empty patch agentset. 
no_patches :: CSTM [AgentRef]
no_patches = return []

-- | Reports true if either boolean1 or boolean2 is true, but not when both are true. 
xor p q = (p || q) && not (p && q)


-- | This reporter gives the maximum x-coordinate for patches, which determines the size of the world. 
max_pxcor :: Monad m => C m Int
max_pxcor = return $ max_pxcor_ conf

-- | This reporter gives the maximum y-coordinate for patches, which determines the size of the world. 
max_pycor :: CSTM Int
max_pycor = return $ max_pycor_ conf

-- | This reporter gives the minimum x-coordinate for patches, which determines the size of the world. 
min_pxcor :: CSTM Int
min_pxcor = return $ min_pxcor_ conf

-- | This reporter gives the maximum y-coordinate for patches, which determines the size of the world. 
min_pycor :: CSTM Int
min_pycor = return $ min_pycor_ conf

-- | This reporter gives the total width of the NetLogo world. 
world_width :: CSTM Int
world_width = return $ (max_pxcor_ conf) - (min_pxcor_ conf) + 1

-- | This reporter gives the total height of the NetLogo world. 
world_height :: CSTM Int
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
clear_all_plots =
    return ()

-- | Clears all lines and stamps drawn by turtles. 
-- | todo
clear_drawing = 
    return ()

{-# INLINE cd #-}
-- | alias for 'clear_drawing'
cd = clear_drawing

-- | Clears all text from the model's output area, if it has one. Otherwise does nothing. 
-- | todo
clear_output =
    return ()

-- | Kills all turtles.
-- Also resets the who numbering, so the next turtle created will be turtle 0.
clear_turtles :: CSTM ()
clear_turtles = do
  (_, tw, _, _, _) <- ask
  (MkWorld ps _ ls) <- lift $ readTVar tw
  lift $ writeTVar tw (MkWorld ps IM.empty ls)

{-# INLINE ct #-}
-- | alias for 'clear_turtles'
ct = clear_turtles

-- | Kills all links.
clear_links :: CSTM ()
clear_links = do
  (_, tw, _, _, _) <- ask
  (MkWorld ps ts _) <- lift $ readTVar tw
  lift $ writeTVar tw (MkWorld ps ts M.empty)

-- | Clears the patches by resetting all patch variables to their default initial values, including setting their color to black. 
clear_patches :: CSTM ()
clear_patches = do
  (_, tw, _, _, _) <- ask
  (MkWorld ps ts _) <- lift $ readTVar tw
  lift $ M.traverseWithKey (\ (x,y) (MkPatch tx ty tc tl tlc to)  -> do
                              writeTVar tc 0
                              writeTVar tl ""
                              writeTVar tlc 9.9
                              mapM_ (flip writeTVar 0) (elems to) -- patches-own to 0
                           ) ps
  return ()

{-# INLINE cp #-}
-- | alias for 'clear_patches'
cp = clear_patches


-- | Clears the tick counter.
-- Does not set the counter to zero. After this command runs, the tick counter has no value. Attempting to access or update it is an error until reset-ticks is called. 
clear_ticks :: CSTM ()
clear_ticks = do
    (gs, _, _, _, _) <- ask
    lift $ writeTVar (gs ! 1) undefined

-- | Resets the tick counter to zero, sets up all plots, then updates all plots (so that the initial state of the world is plotted). 
reset_ticks :: CSTM ()
reset_ticks = do
    (gs, _, _, _, _) <- ask
    lift $ writeTVar (gs ! 1) 0

-- | Advances the tick counter by one and updates all plots. 
tick :: CSTM ()
tick = tick_advance 1

-- | Advances the tick counter by number. The input may be an integer or a floating point number. (Some models divide ticks more finely than by ones.) The input may not be negative. 
-- todo: dynamic typing, float
tick_advance :: Double -> CSTM ()
tick_advance n = do
  (gs, _, _, _, _) <- ask
  lift $ modifyTVar' (gs ! 1) (+n)

-- | Reports the current value of the tick counter. The result is always a number and never negative. 
-- todo: dynamic typing, integer or float
ticks :: CSTM Double
ticks = do
  (gs, _, _, _, _) <- ask
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
one_of :: [a] -> CSTM a
one_of [] = error "empty list"
one_of l = do
  (_,_,_,_,ts) <- ask
  s <- lift $ readTVar ts
  let (v,s') = randomR (0, length l) s
  lift $ writeTVar ts s'
  return (l !! v)

{-# INLINE reduce #-}
-- | Reduces a list from left to right using the given task, resulting in a single value. (foldl)
reduce = foldl

-- | For a list, reports a copy of list with all instances of item removed. 
-- | todo
remove = undefined

{-# INLINE remove_duplicates #-}
-- | Reports a copy of list with all duplicate items removed. The first of each item remains in place. 
remove_duplicates :: Eq a => [a] -> [a]
remove_duplicates = nub

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
      x <- one_of l
      xs <- shuffle' (delete x l) (i-1)
      return $ x:xs

{-# INLINE sort_ #-}
-- | Reports a sorted list of numbers, strings, or agents. 
sort_ :: Ord a => [a] -> [a]
sort_ = sort

{-# INLINE sort_by #-}
-- | If the input is a list, reports a new list containing the same items as the input list, in a sorted order defined by the boolean reporter task. 
-- | todo: requires dynamic typing
sort_by = sortBy

-- | Reports a list of agents, sorted according to each agent's value for reporter. Ties are broken randomly. 
sort_on :: Ord a => CSTM a -> [AgentRef] -> CSTM [AgentRef]
sort_on rep as = do
  (gs, tw, _, p, s) <- ask
  xs <- lift . sequence $ [runReaderT rep (gs, tw, a, p, s) | a <- as]
  let rs = zip xs as
  return $ map snd $ sortBy (compare `on` fst) rs where


-- | Reports just a section of the given list or string, ranging between the first position (inclusive) and the second position (exclusive). 
-- 0-indexed
sublist l x y = take (y-x) . drop x $ l
{-# INLINE substring #-}
-- | Reports just a section of the given list or string, ranging between the first position (inclusive) and the second position (exclusive). 
substring = sublist

-- | From an agentset, reports an agentset of size size randomly chosen from the input set, with no repeats. 
-- | todo
n_of = undefined

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
abs_ = abs

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
subtract_headings = undefined

{-# INLINE sum_ #-}
-- | Reports the sum of the items in the list. 
sum_ = sum

-- | The link makes itself invisible. 
hide_link :: CSTM ()
hide_link = do
  (_, _, LinkRef _ (MkLink {lhiddenp_ = h}), _, _) <- ask
  lift $ writeTVar h True

-- | The turtle becomes visible again. 
show_link :: CSTM ()
show_link = do
  (_, _, LinkRef _ (MkLink {lhiddenp_ = h}), _, _) <- ask
  lift $ writeTVar h False


-- | Reports the distance between the endpoints of the link. 
link_length :: CSTM Double
link_length = do
  (_, tw, LinkRef (f,t) _, _, _) <- ask
  [TurtleRef _ (MkTurtle {xcor_ = fx, ycor_ = fy})] <- turtle f
  [TurtleRef _ (MkTurtle {xcor_ = tx, ycor_ = ty})] <- turtle t
  x <- lift $ readTVar fx
  y <- lift $ readTVar fy
  x' <- lift $ readTVar tx
  y' <- lift $ readTVar ty
  return $ sqrt ((delta x x' (fromIntegral $ max_pxcor_ conf)) ^ 2 + (delta y y' (fromIntegral $ max_pycor_ conf) ^ 2))

-- | Given the who numbers of the endpoints, reports the link connecting the turtles. If there is no such link reports nobody. To refer to breeded links you must use the singular breed form with the endpoints. 
link :: Int -> Int -> CSTM [AgentRef]
link f t = do
  (_, tw,_, _, _) <- ask
  (MkWorld _ _ ls) <- lift $ readTVar tw
  return $ [maybe Nobody (LinkRef (f,t)) $ M.lookup (f,t) ls]

-- | Reports the agentset consisting of all links. 
links :: CSTM [AgentRef]
links = do
  (_,tw,_, _, _) <- ask
  (MkWorld _ _ ls) <- lift $ readTVar tw
  return $ M.foldrWithKey (\ k x ks -> LinkRef k x: ks) [] ls


-- | Report the undirected link between turtle and the caller. If no link exists then it reports nobody. 
link_with :: [AgentRef] -> CSTM [AgentRef]
link_with [TurtleRef x _] = do
  (_, _, TurtleRef y _, _, _) <- ask
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
  (_, _, TurtleRef y _, _, _) <- ask
  lxy <- link x y
  lyx <- link y x
  return $ case (lxy,lyx) of
             ([Nobody], _) -> [Nobody]
             (_, [Nobody]) -> lxy
             ([LinkRef _ _], [LinkRef _ _]) -> error "undirected link"


-- | Reports the directed link from the caller to turtle. If no link exists then it reports nobody. 
out_link_to :: [AgentRef] -> CSTM [AgentRef]
out_link_to [TurtleRef x _] = do
  (_, _, TurtleRef y _, _, _) <- ask
  lxy <- link x y
  lyx <- link y x
  return $ case (lyx,lxy) of
             ([Nobody], _) -> [Nobody]
             (_, [Nobody]) -> lyx
             ([LinkRef _ _], [LinkRef _ _]) -> error "undirected link"


-- | Reports an agentset of all undirected links connected to the caller. 
my_links :: CSTM [AgentRef]
my_links = do
  (_, tw, TurtleRef x _, _, _) <- ask 
  (MkWorld _ _ ls) <- lift $ readTVar tw
  return $ map (uncurry LinkRef) $ M.assocs $ M.intersection (M.filterWithKey (\ (f,_) _ -> f == x) ls) (M.filterWithKey (\ (_,t) _ -> t == x) ls)

-- | Reports an agentset of all the directed links going out from the caller to other nodes. 
my_out_links :: CSTM [AgentRef]
my_out_links = do
  (_, tw, TurtleRef x _, _, _) <- ask 
  (MkWorld _ _ ls) <- lift $ readTVar tw
  return $ map (uncurry LinkRef) $ M.assocs $ M.filterWithKey (\ (f,_) _ -> f == x) ls

-- |  Reports an agentset of all the directed links coming in from other nodes to the caller. 
my_in_links :: CSTM [AgentRef]
my_in_links = do
  (_, tw, TurtleRef x _, _, _) <- ask 
  (MkWorld _ _ ls) <- lift $ readTVar tw
  return $ map (uncurry LinkRef) $ M.assocs $ M.filterWithKey (\ (_,t) _ -> t == x) ls

-- | Reports an empty link agentset. 
no_links :: Monad m => ReaderT Context m [AgentRef]
no_links = return []

-- | Ties end1 and end2 of the link together. If the link is a directed link end1 is the root turtle and end2 is the leaf turtle. The movement of the root turtle affects the location and heading of the leaf turtle. If the link is undirected the tie is reciprocal so both turtles can be considered root turtles and leaf turtles. Movement or change in heading of either turtle affects the location and heading of the other turtle. 
tie :: CSTM ()
tie = do
  (_, _, LinkRef _ (MkLink {tie_mode = t}), _, _) <- ask
  lift $ writeTVar t Fixed

-- | Unties end2 from end1 (sets tie-mode to "none") if they were previously tied together. If the link is an undirected link, then it will untie end1 from end2 as well. It does not remove the link between the two turtles. 
untie :: CSTM ()
untie = do
  (_, _, LinkRef _ (MkLink {tie_mode = t}), _, _) <- ask
  lift $ writeTVar t None



-- | lifting STM to IO, a wrapper to atomically
atomic :: CSTM a -> CIO a
atomic = mapReaderT atomically

-- | The specified agent or agentset runs the given commands. 
ask_ :: CIO a -> [AgentRef] -> CIO ()
ask_ f as = do
 (gs, tw, _, p, s) <- ask
 tg <- lift ThreadGroup.new
 lift . sequence_ $ [ThreadGroup.forkIO tg (runReaderT f (gs, tw, a, p, s)) | a <- as]
 lift $ ThreadGroup.wait tg

-- | For an agent, reports the value of the reporter for that agent (turtle or patch). 
--  For an agentset, reports a list that contains the value of the reporter for each agent in the agentset (in random order). 
of_ :: CIO a -> [AgentRef] -> CIO [a]
of_ f as = do
  (gs, tw, _, p, s) <- ask
  xs <- lift . sequence $ [Thread.forkIO (runReaderT f (gs, tw, a, p, s)) | a <- as]
  lift $ mapM (\(_, wait) -> wait >>= Thread.result ) xs

-- | Takes two inputs: an agentset and a boolean reporter. Reports a new agentset containing only those agents that reported true 
-- in other words, the agents satisfying the given condition. 
with :: CIO Bool -> [AgentRef] -> CIO [AgentRef]
with f as = do
  res <- f `of_` as
  return $ foldr (\ (a, r) l -> if r then (a:l) else l) [] (zip as res)

-- Type-safe Casts

is_turtlep :: (Monad m, Typeable a) => a -> ReaderT Context m Bool
is_turtlep t = return $ maybe False (\ t -> case t of
                                    [TurtleRef _ _] -> True
                                    _ -> False)
                                         (cast t :: Maybe [AgentRef])

is_patchp :: (Monad m, Typeable a) => a -> ReaderT Context m Bool
is_patchp t = return $ maybe False (\ t -> case t of
                                    [PatchRef _ _] -> True
                                    _ -> False)
                                         (cast t :: Maybe [AgentRef])

is_agentp :: (Monad m, Typeable a) => a -> ReaderT Context m Bool
is_agentp t = return $ maybe False (\ t -> case t of -- check for a single agent
                                    [_] -> True
                                    _ -> False) (cast t :: Maybe [AgentRef])

-- alternative but slower implementation is_agentp a = is_turtlep a || is_patchp a

-- | Checks only the 1st element
-- | todo: would require a datatype distinction between agentrefs
is_patch_setp :: (Monad m, Typeable a) => [a] -> ReaderT Context m Bool
is_patch_setp (p:_) = is_patchp p

-- | Checks only the 1st element
-- | todo: would require a datatype distinction between agentrefs
is_turtle_setp :: (Monad m, Typeable a) => [a] -> ReaderT Context m Bool
is_turtle_setp (t:_) = is_turtlep t

-- | Checks only the 1st element
-- | todo: would require a datatype distinction between agentrefs
is_agentsetp :: (Monad m, Typeable a) => [a] -> ReaderT Context m Bool
is_agentsetp (a:_) = do 
  ip <- is_patchp a
  it <- is_turtlep a 
  il <- is_linkp a
  return $ ip || it || il

-- Not used, because EDSL, using internal lambda abstractions
-- is_command_taskp
-- is_reporter_taskp

--is_listp :: (Monad m, Typeable a) => a -> ReaderT Context m Bool
--is_listp :: (Typeable a, Typeable t) => t -> [a]
--is_listp l =  (cast l :: Typeable a => Maybe [a])

is_stringp :: (Monad m, Typeable a) => a -> ReaderT Context m Bool
is_stringp s = return $ maybe False (const True) (cast s :: Maybe String)

is_numberp :: (Monad m, Typeable a) => a -> ReaderT Context m Bool
is_numberp n = return $ is_intp n || is_integerp n || is_floatp n || is_doublep n
               where
                 is_intp n = maybe False (const True) (cast n :: Maybe Int)
                 is_integerp n = maybe False (const True) (cast n :: Maybe Integer)
                 is_floatp n = maybe False (const True) (cast n :: Maybe Float)
                 is_doublep n = maybe False (const True) (cast n :: Maybe Double)

is_linkp :: (Monad m, Typeable a) => a -> ReaderT Context m Bool
is_linkp l = return $ maybe False (\ l -> case l of
                                    [LinkRef _ _] -> True
                                    _ -> False)
                                         (cast l :: Maybe [AgentRef])

is_directed_linkp :: (Monad m, Typeable a) => a -> ReaderT Context m Bool
is_directed_linkp l = return $ maybe False (\ l -> case l of
                                    [LinkRef _ (MkLink {directed_ = d})] -> d
                                    _ -> False)
                                         (cast l :: Maybe [AgentRef])

is_undirected_linkp :: (Monad m, Typeable a) => a -> ReaderT Context m Bool
is_undirected_linkp = liftM not . is_directed_linkp



-- | Checks only the 1st element
-- | todo: would require a datatype distinction between agentrefs
is_link_setp (l:_) = is_linkp l


-- Unsafe
--

unsafe_turtles_here :: CIO [AgentRef]
unsafe_turtles_here = do
  [s] <- self
  h <- unsafe_patch_here
  ts <- unsafe_turtles
  res <- with (return . ( == h) =<< unsafe_patch_here) ts
  return (s:res)

unsafe_turtles_at :: Double -> Double -> CIO [AgentRef]
unsafe_turtles_at x y = do
  (_, _, a, _, _) <- ask
  p <- unsafe_patch_at x y
  with (return . (== [a])  =<< unsafe_patch_here) =<< unsafe_turtles


unsafe_patch_at :: Double -> Double -> CIO [AgentRef]
unsafe_patch_at x y = do
  (_, _, a, _, _) <- ask
  case a of
    PatchRef (px, py) _ -> unsafe_patch (fromIntegral px) (fromIntegral py)
    TurtleRef _ _ -> do
                 [PatchRef (px, py) _] <- unsafe_patch_here
                 unsafe_patch (fromIntegral px + x) (fromIntegral py +y)
                 

unsafe_patches :: CIO [AgentRef]
unsafe_patches = do
  (_,tw,_, _, _) <- ask
  (MkWorld ps _ _) <- lift $ readTVarIO tw
  return $ M.foldrWithKey (\ k x ks -> PatchRef k x: ks) [] ps


unsafe_patch :: Double -> Double -> CIO [AgentRef]
unsafe_patch x y = do
  (_, tw,_, _, _) <- ask
  (MkWorld ps _ _) <- lift $ readTVarIO tw
  return $ if x' > max_pxcor_ conf || x' < min_pxcor_ conf || y' > max_pycor_ conf || y' < min_pycor_ conf
           then [Nobody]
           else
               [PatchRef (x',y') (ps M.! (x',y'))]
         where
           x' = round x
           y' = round y

unsafe_turtles :: CIO [AgentRef]
unsafe_turtles = do
  (_,tw,_, _, _) <- ask
  (MkWorld _ ts _) <- lift $ readTVarIO tw
  return $ IM.foldrWithKey (\ k x ks -> TurtleRef k x: ks) [] ts

unsafe_patch_here :: CIO [AgentRef]
unsafe_patch_here = do
  (_,_,TurtleRef _ (MkTurtle {xcor_ = x, ycor_ = y}), _, _) <- ask
  x' <- lift $ readTVarIO x
  y' <- lift $ readTVarIO y
  unsafe_patch x' y'

unsafe_turtle :: Int -> CIO [AgentRef]
unsafe_turtle n = do
  (_, tw,_, _, _) <- ask
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
  (_,_,TurtleRef _ (MkTurtle {heading_ = h}), _, _) <- ask
  lift $ readTVarIO h

unsafe_xcor :: CIO Double
unsafe_xcor = do
  (_,_,TurtleRef _ (MkTurtle {xcor_ = x}), _, _) <- ask
  lift $ readTVarIO x

unsafe_ycor :: CIO Double
unsafe_ycor = do
  (_,_,TurtleRef _ (MkTurtle {ycor_ = y}), _, _) <- ask
  lift $ readTVarIO y

unsafe_color :: CIO Double
unsafe_color = do
  (_,_,TurtleRef _ (MkTurtle {color_ = c}), _, _) <- ask
  lift $ readTVarIO c


unsafe_random_xcor :: CIO Double
unsafe_random_xcor = lift $ getStdRandom $ randomR ((fromIntegral $ min_pxcor_ conf),(fromIntegral $ max_pxcor_ conf))

unsafe_random_ycor :: CIO Double
unsafe_random_ycor = lift $ getStdRandom $ randomR ((fromIntegral $ min_pycor_ conf),(fromIntegral $ max_pycor_ conf))

unsafe_random_pxcor :: CIO Int
unsafe_random_pxcor = lift $ getStdRandom $ randomR (min_pxcor_ conf, max_pxcor_ conf)

unsafe_random_pycor :: CIO Int
unsafe_random_pycor = lift $ getStdRandom $ randomR (min_pycor_ conf, max_pycor_ conf)

unsafe_random :: Int -> CIO Int
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
unsafe_turtles_on :: [AgentRef] -> CIO [AgentRef]
unsafe_turtles_on [] = return []
unsafe_turtles_on ps@(PatchRef _ _ : _) = do
  with (liftM (flip elem ps . head) unsafe_patch_here) =<< unsafe_turtles
unsafe_turtles_on ts@(TurtleRef _ _ : _) = do
  unsafe_turtles_on =<< of_ (liftM head unsafe_patch_here) ts

unsafe_right :: Double -> CIO Double
unsafe_right n = do
  h <- unsafe_heading
  return $ mod_ (h + n) 360

unsafe_left :: Double -> CIO Double
unsafe_left n = do
  unsafe_right (-n)


unsafe_distance :: [AgentRef] -> CIO Double
unsafe_distance [PatchRef (x,y) _] = do
  unsafe_distancexy (fromIntegral x) (fromIntegral y)
unsafe_distance [TurtleRef _ (MkTurtle {xcor_ = tx, ycor_ = ty})] = do
  x <- lift $ readTVarIO tx
  y <- lift $ readTVarIO ty
  unsafe_distancexy x y

unsafe_distancexy :: Double -> Double -> CIO Double
unsafe_distancexy x' y' = do
  (_,_,ref,_,_) <- ask
  (x,y) <- case ref of
            PatchRef (x,y) _ -> return (fromIntegral x, fromIntegral y)
            TurtleRef _ (MkTurtle {xcor_ = tx, ycor_ = ty}) -> liftM2 (,) (lift $ readTVarIO tx) (lift $ readTVarIO ty)
  return $ sqrt ((delta x x' (fromIntegral $ max_pxcor_ conf)) ^ 2 + (delta y y' (fromIntegral $ max_pycor_ conf) ^ 2))
      where
        delta a1 a2 aboundary = min (abs (a2 - a1)) (abs (a2 + a1) + 1)

-- | todo
unsafe_downhill = undefined

-- | todo
unsafe_downhill4 = undefined

-- | todo
unsafe_towards = undefined

-- | todo
unsafe_towardsxy = undefined

unsafe_in_radius :: [AgentRef] -> Double -> CIO [AgentRef]
unsafe_in_radius as n = do
  (_,_,ref,_,_) <- ask
  (x, y) <- case ref of
    PatchRef (x,y) _ -> return $ (fromIntegral x, fromIntegral y)
    TurtleRef _ (MkTurtle {xcor_ = tx, ycor_ = ty}) -> liftM2 (,) (lift $ readTVarIO tx) (lift $ readTVarIO ty)
  with (unsafe_distancexy x y >>= \ d -> return $ d <= n) as

unsafe_in_cone = undefined

unsafe_no_turtles :: CIO [AgentRef]
unsafe_no_turtles = return []

unsafe_no_patches :: CIO [AgentRef]
unsafe_no_patches = return []

-- | Runs the given commands only if it's been more than number seconds since the last time this agent ran them in this context. Otherwise, the commands are skipped. 
-- | NB: Works differently than NetLogo, in that only the calling thread is suspended, not the whole simulation
unsafe_every :: Double -> CIO a -> CIO ()
unsafe_every n a = a >> unsafe_wait n

-- | Wait the given number of seconds. (This needn't be an integer; you can specify fractions of seconds.) Note that you can't expect complete precision; the agent will never wait less than the given amount, but might wait slightly more. 
-- | NB: Works differently than NetLogo, in that only the calling thread is suspended, not the whole simulation
unsafe_wait n = lift $ threadDelay (round $ n * 1000000)

unsafe_ticks :: CIO Double
unsafe_ticks = do
  (gs, _, _, _, _) <- ask
  lift $ readTVarIO (gs ! 1)

unsafe_one_of :: [a] -> CIO a
unsafe_one_of [] = error "empty list"
unsafe_one_of l = do
  v <- lift $ randomRIO (0, length l)
  return (l !! v)

-- | todo optimize with arrays <http://www.haskell.org/haskellwiki/Random_shuffle>
unsafe_shuffle :: Eq a => [a] -> CIO [a]
unsafe_shuffle [] = return []
unsafe_shuffle l = shuffle' l (length l) where
    shuffle [x] 1 = return [x]
    shuffle' l i = do
      x <- unsafe_one_of l
      xs <- shuffle' (delete x l) (i-1)
      return $ x:xs


unsafe_show_ :: Show a => a -> CIO ()
unsafe_show_ a = do
  (_,_, r, _, _) <- ask
  lift $ putStrLn $ (case r of
                           ObserverRef -> "observer: "
                           PatchRef (x,y) _ -> "(patch " ++ show x ++ " " ++ show y ++ "): "
                           LinkRef (x,y) _ -> "(link " ++ show x ++ " " ++ show y ++ "): "
                           TurtleRef i _ -> "(turtle " ++ show i ++ "): ")   ++ show a


unsafe_print_ :: Show a => a -> CIO ()
unsafe_print_ a = do
  lift $ putStrLn $ show a



