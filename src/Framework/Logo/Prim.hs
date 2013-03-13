module Framework.Logo.Prim (
                           -- * Agent related
                           self, other, count, distance, distancexy, towards, towardsxy, in_radius, in_cone,

                           -- * Turtle related
                           turtles_here, turtles_at, jump, setxy, forward, fd, back, bk, create_turtles, crt, create_ordered_turtles, cro, turtles, turtle, turtle_set, face, xcor, ycor, who, dx, dy, home, right, rt, left, lt, downhill, downhill4,  hide_turtle, ht, show_turtle, st, pen_down, pd, pen_up, pu, pen_erase, pe, no_turtles,


                           -- * Patch related
                           patch_at, patch_here, patch_ahead, patches, patch, patch_set, can_movep, heading, no_patches,

                           -- * Random related
                           random_xcor, random_ycor,random_pxcor, random_pycor, random, random_float,

                           -- * Color
                           primary_colors, black, white, gray, red, orange, brown, yellow, green, lime, turquoise, cyan, sky, blue, violet, magenta, pink,

                           -- * List related
                           sum_, anyp, item, one_of, remove, remove_item, replace_item, shuffle, sublist, substring, n_of, but_first, but_last, emptyp, first, foreach, fput, last_, length_, list, lput, map_, memberp, position, reduce, remove_duplicates, reverse_, sentence, sort_, sort_by, sort_on, max_, min_,n_values,

                           -- * Math
                           xor, e, exp_, pi_, cos_, sin_, tan_, mod_, acos_, asin_, atan_, int, log_, mean, median, modes, variance, standard_deviation, subtract_headings, abs_, floor_, ceiling_, remainder, round_, sqrt_, 

                           -- * Misc
                           max_pxcor, max_pycor, min_pxcor, min_pycor, world_width, world_height, clear_all, ca, clear_all_plots, clear_drawing, cd, clear_output, clear_turtles, ct, clear_patches, cp, clear_ticks, reset_ticks, tick, tick_advance, ticks, histogram, 

                           -- * Input/Output
                           show_, print_, read_from_string,

                           -- * IO Operations
                           atomic, ask_, of_, with


) where

import Framework.Logo.Base
import Framework.Logo.Conf
import Control.Concurrent.STM
import Control.Monad.Reader
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

-- |  Reports this turtle or patch. 
self :: CSTM [AgentRef] -- ^ returns a list (set) of agentrefs to be compatible with the 'turtle-set' function
self = do
  (_, _, a, _, _) <- ask
  return [a]

-- |  Reports an agentset which is the same as the input agentset but omits this agent. 
other :: [AgentRef] -> CSTM [AgentRef]
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

-- | Internal
random_primary_color :: CSTM Double
random_primary_color = do
  (_,_,_,_,ts) <- ask
  s <- lift $ readTVar ts
  let (v,s') = randomR (0,13) s
  lift $ writeTVar ts s'
  return (primary_colors !! v)

-- | Reports the number of agents in the given agentset. 
count :: [AgentRef] -> CSTM Int
count = return . length

-- | Reports true if the given agentset is non-empty, false otherwise. 
anyp :: [AgentRef] -> CSTM Bool
anyp as = return $ not $ null as


-- | Internal
newTurtle x = MkTurtle <$>
  newTVar x <*>
  newTVar "turtles" <*>
  newTVar 9.9 <*>
  newTVar 0 <*>
  newTVar 0 <*>
  newTVar 0 <*>
  newTVar "default" <*>
  newTVar "" <*>
  newTVar 9.9 <*>
  newTVar False <*>
  newTVar 1 <*>
  newTVar 1 <*>
  newTVar Up

-- | Reports the agentset consisting of all patches. 
patches :: CSTM [AgentRef]
patches = do
  (_,tw,_, _, _) <- ask
  (MkWorld ps _) <- lift $ readTVar tw
  return $ M.foldrWithKey (\ k x ks -> PatchRef k x: ks) [] ps



-- | Given the x and y coordinates of a point, reports the patch containing that point. 
patch :: Double -> Double -> CSTM [AgentRef]
patch x y = do
  (_, tw,_, _, _) <- ask
  (MkWorld ps _) <- lift $ readTVar tw
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
  lift $ modifyTVar x (+ (sin_ h' * n)) >>  modifyTVar y (+ (cos_ h' * n))



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

-- | Creates number new turtles at the origin. New turtles have random integer headings and the color is randomly selected from the 14 primary colors. 
create_turtles :: Int -> CSTM [AgentRef]
create_turtles n = do
  (gs, tw, _, _, _) <- ask
  let who = gs ! 0
  lift $ do 
    oldWho <- liftM round $ readTVar who
    modifyTVar who (\ ow -> fromIntegral n + ow)
    ns <- newTurtles oldWho n
    modifyTVar tw (addTurtles ns) 
    return $ map (uncurry TurtleRef) $ IM.toList ns -- todo: can be optimized
        where
                      newTurtles w n = return . IM.fromAscList =<< sequence [do
                                                                              t <- newTurtle i
                                                                              return (i, t)
                                                                             | i <- [w..w+n-1]]
                      addTurtles ts' (MkWorld ps ts)  = MkWorld ps (ts `IM.union` ts')

{-# INLINE crt #-}
-- | alias for 'create_turtles'
crt = create_turtles

-- |  Creates number new turtles. New turtles start at position (0, 0), are created with the 14 primary colors, and have headings from 0 to 360, evenly spaced. 
create_ordered_turtles :: Int -> CSTM [AgentRef]
create_ordered_turtles = undefined

{-# INLINE cro #-}
-- | alias for 'create_ordered_turtles'
cro = create_ordered_turtles

-- | Reports the agentset consisting of all turtles. 
turtles :: CSTM [AgentRef]
turtles = do
  (_,tw,_, _, _) <- ask
  MkWorld _ ts <- lift $ readTVar tw
  return $ IM.foldrWithKey (\ k x ks -> TurtleRef k x: ks) [] ts

-- | Reports the turtle with the given who number, or nobody if there is no such turtle. For breeded turtles you may also use the single breed form to refer to them. 
turtle :: Int -> CSTM [AgentRef]
turtle n = do
  (_, tw,_, _, _) <- ask
  (MkWorld _ ts) <- lift $ readTVar tw
  return [TurtleRef n (ts IM.! n)]


-- | Reports an agentset containing all of the turtles anywhere in any of the inputs.
turtle_set :: [CSTM [AgentRef]] -> CSTM [AgentRef]
turtle_set ts = sequence ts >>= return . concat

-- | Reports an agentset containing all of the patches anywhere in any of the inputs. 
patch_set = turtle_set

-- | Reports true if this turtle can move distance in the direction it is facing without violating the topology; reports false otherwise. 
can_movep :: Double -> CSTM Bool
can_movep n = patch_ahead n >>= \ p -> return (p /= [Nobody])


-- | This is a built-in turtle variable. It indicates the direction the turtle is facing. 
heading :: CSTM Double
heading = do
  (_,_,TurtleRef _ (MkTurtle {heading_ = h}), _, _) <- ask
  lift $ readTVar h

-- | This is a built-in turtle variable. It holds the current x coordinate of the turtle. 
xcor :: CSTM Double
xcor = do
  (_,_,TurtleRef _ (MkTurtle {xcor_ = x}), _, _) <- ask
  lift $ readTVar x

-- | This is a built-in turtle variable. It holds the current y coordinate of the turtle.
ycor :: CSTM Double
ycor = do
  (_,_,TurtleRef _ (MkTurtle {ycor_ = y}), _, _) <- ask
  lift $ readTVar y

-- | This is a built-in turtle variable. It holds the turtle's "who number" or ID number, an integer greater than or equal to zero. You cannot set this variable; a turtle's who number never changes. 
who :: CSTM Int
who = do
  (_,_,TurtleRef i _, _, _) <- ask
  return i

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
-- | todo
face = undefined

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
max_pxcor :: CSTM Int
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
  (MkWorld ps _) <- lift $ readTVar tw
  lift $ writeTVar tw (MkWorld ps IM.empty)

{-# INLINE ct #-}
-- | alias for 'clear_turtles'
ct = clear_turtles

-- | Clears the patches by resetting all patch variables to their default initial values, including setting their color to black. 
clear_patches :: CSTM ()
clear_patches = do
  (_, tw, _, _, _) <- ask
  (MkWorld ps ts) <- lift $ readTVar tw
  lift $ M.traverseWithKey (\ (x,y) (MkPatch tx ty tc tl tlc)  -> do
                              writeTVar tc 0
                              writeTVar tl ""
                              writeTVar tlc 9.9) ps
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
