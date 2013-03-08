module Framework.Logo.Prim where

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
import System.Random
import Control.Monad (forM_)

self :: CSTM [AgentRef]
self = do
  (_, _, a, _, _) <- ask
  return [a]

other as = do
  [s] <- self
  return $ delete s as

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

turtles_at :: Double -> Double -> CSTM [AgentRef]
turtles_at x y = do
  (_, _, a, _, _) <- ask
  [PatchRef (px, py) _] <- patch_at x y
  ts <- turtles
  filterM (\ (TurtleRef _ (MkTurtle {xcor_ = x, ycor_ = y})) -> do 
             x' <- lift $ readTVar x
             y' <- lift $ readTVar y
             return $ round x' == px && round y' == py
          ) ts



atomic :: CSTM a -> CIO a
atomic = mapReaderT atomically


ask_ :: CIO a -> [AgentRef] -> CIO ()
ask_ f as = do
 (gs, tw, _, p, s) <- ask
 tg <- lift ThreadGroup.new
 lift . sequence_ $ [ThreadGroup.forkIO tg (runReaderT f (gs, tw, a, p, s)) | a <- as]
 lift $ ThreadGroup.wait tg

of_ :: CIO a -> [AgentRef] -> CIO [a]
of_ f as = do
  (gs, tw, _, p, s) <- ask
  xs <- lift . sequence $ [Thread.forkIO (runReaderT f (gs, tw, a, p, s)) | a <- as]
  lift $ mapM (\(_, wait) -> wait >>= Thread.result ) xs

with :: CIO Bool -> [AgentRef] -> CIO [AgentRef]
with f as = do
  res <- f `of_` as
  return $ foldr (\ (a, r) l -> if r then (a:l) else l) [] (zip as res)

show_ :: Show a => a -> CSTM ()
show_ a = do
  (_,_, r, p, _) <- ask
  lift $ writeTChan p $ (case r of
                           ObserverRef -> "observer: "
                           PatchRef (x,y) _ -> "(patch " ++ show x ++ " " ++ show y ++ "): "
                           TurtleRef i _ -> "(turtle " ++ show i ++ "): ")   ++ show a

print_ :: Show a => a -> CSTM ()
print_ a = do
  (_,_, _, p, _) <- ask
  lift $ writeTChan p $ show a
                           

patch_at :: Double -> Double ->  CSTM [AgentRef]
patch_at x y = do
  (_, _, a, _, _) <- ask
  case a of
    PatchRef (px, py) _ -> patch (fromIntegral px) (fromIntegral py)
    TurtleRef _ _ -> do
                 [PatchRef (px, py) _] <- patch_here
                 patch (fromIntegral px + x) (fromIntegral py +y)

patch_here :: CSTM [AgentRef]
patch_here = do
  (_,_,TurtleRef _ (MkTurtle {xcor_ = x, ycor_ = y}), _, _) <- ask
  x' <- lift $ readTVar x
  y' <- lift $ readTVar y
  patch x' y'


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

black = 0
white = 9.9
gray = 5
red = 15
orange = 25
brown = 35
yellow = 45
green = 55
lime = 65
turquoise = 75
cyan = 85
sky = 95
blue = 105
violet = 115
magenta = 125
pink = 135

-- approximate-rgb

primary_colors :: [Double]
primary_colors = [gray, red, orange, brown, yellow, green, lime, turquoise, cyan, sky, blue, violet, magenta, pink]

random_primary_color :: CSTM Double
random_primary_color = do
  (_,_,_,_,ts) <- ask
  s <- lift $ readTVar ts
  let (v,s') = randomR (0,13) s
  lift $ writeTVar ts s'
  return (primary_colors !! v)

count :: [a] -> CSTM Int
count = return . length

anyp :: [AgentRef] -> CSTM Bool
anyp as = return $ not $ null as


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

patches :: CSTM [AgentRef]
patches = do
  (_,tw,_, _, _) <- ask
  (MkWorld ps _) <- lift $ readTVar tw
  return $ M.foldrWithKey (\ k x ks -> PatchRef k x: ks) [] ps


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


jump :: Double -> CSTM ()
jump n = do
  (_,_, TurtleRef _ (MkTurtle {xcor_ = x, ycor_ = y, heading_ = h}), _, _) <- ask
  h' <- lift $ readTVar h
  lift $ modifyTVar x (+ (sin_ h' * n)) >>  modifyTVar y (+ (cos_ h' * n))



setxy :: Double -> Double -> CSTM ()
setxy x' y' = do
  (_,_, TurtleRef _ (MkTurtle {xcor_ = x, ycor_ = y}), _, _) <- ask
  lift $ writeTVar x x' >> writeTVar y y'


forward :: Double -> CSTM ()
forward n | n == 0 = return ()
          | n > 1 = jump 1 >> forward (n-1)
          | n < -1 = jump (-1) >> forward (n+1)
          | (0 < n && n <= 1) || (-1 <= n && n < 0) = jump n

back :: Double -> CSTM ()
back n = forward (-n)
bk = back


create_turtles :: Int -> CSTM [AgentRef]
create_turtles n = do
  (gs, tw, _, _, _) <- ask
  let who = gs ! 0
  lift $ do 
    oldWho <- readTVar who
    modifyTVar who (n +)
    ns <- newTurtles oldWho n
    modifyTVar tw (addTurtles ns) 
    return $ map (uncurry TurtleRef) $ IM.toList ns -- todo: can be optimized
        where
                      newTurtles w n = return . IM.fromAscList =<< sequence [do
                                                                              t <- newTurtle i
                                                                              return (i, t)
                                                                             | i <- [w..w+n-1]]
                      addTurtles ts' (MkWorld ps ts)  = MkWorld ps (ts `IM.union` ts')

create_ordered_turtles :: Int -> CSTM [AgentRef]
create_ordered_turtles = undefined

turtles :: CSTM [AgentRef]
turtles = do
  (_,tw,_, _, _) <- ask
  MkWorld _ ts <- lift $ readTVar tw
  return $ IM.foldrWithKey (\ k x ks -> TurtleRef k x: ks) [] ts

turtle :: Int -> CSTM [AgentRef]
turtle n = do
  (_, tw,_, _, _) <- ask
  (MkWorld _ ts) <- lift $ readTVar tw
  return [TurtleRef n (ts IM.! n)]


turtle_set :: [CSTM [AgentRef]] -> CSTM [AgentRef]
turtle_set ts = sequence ts >>= return . concat

patch_set = turtle_set

can_movep :: Double -> CSTM Bool
can_movep n = patch_ahead n >>= \ p -> return (p /= [Nobody])


heading :: CSTM Double
heading = do
  (_,_,TurtleRef _ (MkTurtle {heading_ = h}), _, _) <- ask
  lift $ readTVar h

xcor :: CSTM Double
xcor = do
  (_,_,TurtleRef _ (MkTurtle {xcor_ = x}), _, _) <- ask
  lift $ readTVar x

ycor :: CSTM Double
ycor = do
  (_,_,TurtleRef _ (MkTurtle {ycor_ = y}), _, _) <- ask
  lift $ readTVar y

who :: CSTM Int
who = do
  (_,_,TurtleRef i _, _, _) <- ask
  return i

dx :: CSTM Double
dx = liftM sin_ heading

dy :: CSTM Double
dy = liftM cos_ heading

random_xcor :: CSTM Double
random_xcor = do
  (_,_,_,_,ts) <- ask
  s <- lift $ readTVar ts
  let (v, s') = randomR ((fromIntegral $ min_pxcor_ conf),(fromIntegral $ max_pxcor_ conf)) s
  lift $ writeTVar ts s'
  return v

random_ycor :: CSTM Double
random_ycor = do
  (_,_,_,_,ts) <- ask
  s <- lift $ readTVar ts
  let (v, s') = randomR ((fromIntegral $ min_pycor_ conf),(fromIntegral $ max_pycor_ conf)) s
  lift $ writeTVar ts s'
  return v

random_pxcor :: CSTM Int
random_pxcor = do
  (_,_,_,_,ts) <- ask
  s <- lift $ readTVar ts
  let (v, s') = randomR (min_pxcor_ conf, max_pxcor_ conf) s
  lift $ writeTVar ts s'
  return v

random_pycor :: CSTM Int
random_pycor = do
  (_,_,_,_,ts) <- ask
  s <- lift $ readTVar ts
  let (v, s') = randomR (min_pycor_ conf, max_pycor_ conf) s
  lift $ writeTVar ts s'
  return v

random               :: (Random a , Eq a, Ord a, Num a) => a -> CSTM a
random x | x == 0     = return 0
         | otherwise = do
  (_,_,_,_,ts) <- ask
  s <- lift $ readTVar ts
  let (v, s') = randomR (if x < 0 then (x, 0) else (0,x)) s
  lift $ writeTVar ts s'
  return v

random_float               :: Double -> CSTM Double
random_float x | x == 0     = return 0
         | otherwise = do
  (_,_,_,_,ts) <- ask
  s <- lift $ readTVar ts
  let (v, s') = randomR (if x < 0 then (x, 0) else (0,x)) s
  lift $ writeTVar ts s'
  return v


home :: CSTM ()
home = setxy 0 0

right :: Double -> CSTM Double
right n = do
  h <- heading
  return $ mod_ (h + n) 360

left :: Double -> CSTM Double
left n = do
  right (-n)

distance :: [AgentRef] -> CSTM Double
distance [PatchRef (x,y) _] = do
  distancexy (fromIntegral x) (fromIntegral y)
distance [TurtleRef _ (MkTurtle {xcor_ = tx, ycor_ = ty})] = do
  x <- lift $ readTVar tx
  y <- lift $ readTVar ty
  distancexy x y

delta a1 a2 aboundary =
    min (abs (a2 - a1)) (abs (a2 + a1) + 1)

distancexy :: Double -> Double -> CSTM Double
distancexy x' y' = do
  (_,_,ref,_,_) <- ask
  (x,y) <- case ref of
            PatchRef (x,y) _ -> return (fromIntegral x, fromIntegral y)
            TurtleRef _ (MkTurtle {xcor_ = tx, ycor_ = ty}) -> liftM2 (,) (lift $ readTVar tx) (lift $ readTVar ty)
  return $ sqrt ((delta x x' (fromIntegral $ max_pxcor_ conf)) ^ 2 + (delta y y' (fromIntegral $ max_pycor_ conf) ^ 2))


downhill = undefined                    -- todo

downhill4 = undefined                   -- todo

face = 3                 -- todo

towards = undefined                     -- todo

towardsxy = undefined                   -- todo


hide_turtle :: CSTM ()
hide_turtle = do
  (_,_,TurtleRef _ (MkTurtle {hiddenp_ = th}), _, _) <- ask
  lift $ writeTVar th True

show_turtle :: CSTM ()
show_turtle = do
  (_,_,TurtleRef _ (MkTurtle {hiddenp_ = th}), _, _) <- ask
  lift $ writeTVar th False


pen_down :: CSTM ()
pen_down = do
  (_,_,TurtleRef _ (MkTurtle {pen_mode_ = tp}), _, _) <- ask
  lift $ writeTVar tp Down

pen_up :: CSTM ()
pen_up = do
  (_,_,TurtleRef _ (MkTurtle {pen_mode_ = tp}), _, _) <- ask
  lift $ writeTVar tp Up

pen_erase :: CSTM ()
pen_erase = do
  (_,_,TurtleRef _ (MkTurtle {pen_mode_ = tp}), _, _) <- ask
  lift $ writeTVar tp Erase

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

in_cone = undefined

no_turtles :: CSTM [AgentRef]
no_turtles = return []

no_patches :: CSTM [AgentRef]
no_patches = return []

xor p q = (p || q) && not (p && q)


max_pxcor :: CSTM Int
max_pxcor = return $ max_pxcor_ conf

max_pycor :: CSTM Int
max_pycor = return $ max_pycor_ conf

min_pxcor :: CSTM Int
min_pxcor = return $ min_pxcor_ conf

min_pycor :: CSTM Int
min_pycor = return $ min_pycor_ conf

world_width :: CSTM Int
world_width = return $ (max_pxcor_ conf) - (min_pxcor_ conf) + 1

world_height :: CSTM Int
world_height = return $ (max_pycor_ conf) - (min_pycor_ conf) + 1


clear_all :: CSTM ()
clear_all = do
  clear_ticks
  clear_turtles
  clear_patches
  clear_drawing
  clear_all_plots
  clear_output

-- todo
clear_all_plots =
    return ()

-- todo
clear_drawing = 
    return ()

-- todo
clear_output =
    return ()

clear_turtles :: CSTM ()
clear_turtles = do
  (_, tw, _, _, _) <- ask
  (MkWorld ps _) <- lift $ readTVar tw
  lift $ writeTVar tw (MkWorld ps IM.empty)

clear_patches :: CSTM ()
clear_patches = do
  (_, tw, _, _, _) <- ask
  (MkWorld ps ts) <- lift $ readTVar tw
  lift $ M.traverseWithKey (\ (x,y) (MkPatch tx ty tc tl tlc)  -> do
                              writeTVar tc 0
                              writeTVar tl ""
                              writeTVar tlc 9.9) ps
  return ()


clear_ticks :: CSTM ()
clear_ticks = do
    (gs, _, _, _, _) <- ask
    lift $ writeTVar (gs ! 1) undefined

reset_ticks :: CSTM ()
reset_ticks = do
    (gs, _, _, _, _) <- ask
    lift $ writeTVar (gs ! 1) 0

tick :: CSTM ()
tick = tick_advance 1

tick_advance :: Int -> CSTM ()
tick_advance n = do
  (gs, _, _, _, _) <- ask
  lift $ modifyTVar' (gs ! 1) (+n)

ticks :: CSTM Int
ticks = do
  (gs, _, _, _, _) <- ask
  lift $ readTVar (gs ! 1)

but_first = tail
but_last = init
emptyp = null
first = head
--foreach = forM_
fput = (:)

-- todo
histogram = undefined

-- 1-indexed as Erlang, Netlogo
item i l = l !! (i-1)

-- last
-- length

list x y = [x,y]

lput x l = l ++ [x]

-- map

--memberp = elem

-- todo
modes = undefined

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

position = find

one_of :: [a] -> CSTM a
one_of [] = error "empty list"
one_of l = do
  (_,_,_,_,ts) <- ask
  s <- lift $ readTVar ts
  let (v,s') = randomR (0, length l) s
  lift $ writeTVar ts s'
  return (l !! v)

-- reduce = foldl

-- todo
remove = undefined


-- remove_duplicates = nub

-- todo
remove_item = undefined

-- todo
replace_item = undefined

-- reverse

-- sentence
-- no dynamic_typing


-- todo optimize with arrays <http://www.haskell.org/haskellwiki/Random_shuffle>
shuffle :: Eq a => [a] -> CSTM [a]
shuffle [] = return []
shuffle l = shuffle' l (length l) where
    shuffle [x] 1 = return [x]
    shuffle' l i = do
      x <- one_of l
      xs <- shuffle' (delete x l) (i-1)
      return $ x:xs

-- sort
-- sort_by = sortBy

sort_on rep as = undefined

-- 0-indexed
sublist l x y = take (y-x) . drop x $ l
substring = sublist

-- todo
n_of = undefined

-- stringp
-- no dynamic typing

-- read_from_string = read

-- word
-- no dynamic typing

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
-- abs

e = exp 1

pi_ = pi

sin_ = sin . toRadians

cos_ = cos . toRadians

toRadians deg = deg * pi / 180

toDegrees rad = rad * 180 / pi

x `mod_` y | x == 0 = 0
           | otherwise =  fromIntegral (x' `mod` y) + (x - fromIntegral x')
           where x' = floor x

acos_ = toDegrees . acos

asin_ = toDegrees . asin

atan_ x y = toDegrees $ atan2 (toRadians x) (toRadians y)

tan_ = tan . toRadians

-- floor
-- ceiling

int n = truncate n

-- numberp 
-- no dynamic typing

ln n = log n

log_ = flip logBase


-- max
-- min

mean l = let (t,n) = foldl' (\(b,c) a -> (a+b,c+1)) (0,0) l in realToFrac(t)/realToFrac(n)

median l = let (d, m) = (length l) `divMod` 2
           in case m of
                1 -> l !! d
                0 -> (l !! d + l !! (d-1)) / 2

-- rem = remaind
-- round
-- sqrt

-- todo
standard_deviation l = undefined

-- todo 
subtract_headings = undefined

-- sum

-- todo
variance = undefined
