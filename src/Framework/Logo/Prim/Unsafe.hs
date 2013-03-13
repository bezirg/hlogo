-- | This module contains \"unsafe\" counterparts of some common functions taken from the "Framework.Logo.Prim" module. Also, it includes any primitives that cannot be specified in CSTM monad, and thus have to be executed on the CIO monad.
-- Using the functions from this module, will in principle speed up the execution, compared to the Prim module.
module Framework.Logo.Prim.Unsafe (
                           -- * Agent related
                           self, other, count, distance, distancexy, towards, towardsxy, in_radius, in_cone,

                           -- * Turtle related
                           turtles_here, turtles_at, turtles, turtle, turtle_set, face, can_movep, dx, dy, heading, xcor, ycor, who, breed, no_turtles, turtles_on, left, right, downhill, downhill4,

                           -- * Patch related
                           patch_at, patch_here, patch_ahead, patches, patch, patch_set, no_patches,

                           -- * Random related
                           -- | NB: The following random functions use internally 'getStdRandom', which is non thread-safe. This means that it is possible for simultaneous threads to yield the same random number.
                           random_xcor, random_ycor, random_pxcor, random_pycor, random, random_float, new_seed, random_seed, random_exponential, random_gamma, random_normal, random_poisson,
                           -- * Color

                           -- * List related
                           anyp, one_of, shuffle,

                           -- * Math

                           -- * Misc
                           every, wait, max_pxcor, max_pycor, min_pxcor, min_pycor, world_width, world_height, ticks,

                           -- * Input/Output
                           -- | NB: Non-deterministic IO; use with care. Possible output mangling.
                           show_, print_,

                           -- * IO Operations
                           ask_, of_, with
) where

import Framework.Logo.Prim (mod_, sin_, cos_, primary_colors, xor)
import Framework.Logo.Base
import Framework.Logo.Conf
import Control.Concurrent.STM
import Control.Concurrent (threadDelay)
import Control.Monad.Reader
import qualified Control.Concurrent.Thread as Thread
import qualified Control.Concurrent.Thread.Group as ThreadGroup
import Data.List
import qualified Data.IntMap as IM
import qualified Data.Map as M
import Data.Array
import System.Random hiding (random)
import Control.Applicative

count :: [a] -> CIO Int
count = return . length

anyp :: [AgentRef] -> CIO Bool
anyp as = return $ not $ null as

self :: CIO [AgentRef]
self = do
  (_, _, a, _, _) <- ask
  return [a]

other :: [AgentRef] -> CIO [AgentRef]
other as = do
  [s] <- self
  return $ delete s as

turtles_here :: CIO [AgentRef]
turtles_here = do
  [s] <- self
  h <- patch_here
  ts <- turtles
  res <- with (return . ( == h) =<< patch_here) ts
  return (s:res)

turtles_at :: Double -> Double -> CIO [AgentRef]
turtles_at x y = do
  (_, _, a, _, _) <- ask
  p <- patch_at x y
  with (return . (== [a])  =<< patch_here) =<< turtles


patch_at :: Double -> Double -> CIO [AgentRef]
patch_at x y = do
  (_, _, a, _, _) <- ask
  case a of
    PatchRef (px, py) _ -> patch (fromIntegral px) (fromIntegral py)
    TurtleRef _ _ -> do
                 [PatchRef (px, py) _] <- patch_here
                 patch (fromIntegral px + x) (fromIntegral py +y)
                 

patches :: CIO [AgentRef]
patches = do
  (_,tw,_, _, _) <- ask
  (MkWorld ps _) <- lift $ readTVarIO tw
  return $ M.foldrWithKey (\ k x ks -> PatchRef k x: ks) [] ps


patch :: Double -> Double -> CIO [AgentRef]
patch x y = do
  (_, tw,_, _, _) <- ask
  (MkWorld ps _) <- lift $ readTVarIO tw
  return $ if x' > max_pxcor_ conf || x' < min_pxcor_ conf || y' > max_pycor_ conf || y' < min_pycor_ conf
           then [Nobody]
           else
               [PatchRef (x',y') (ps M.! (x',y'))]
         where
           x' = round x
           y' = round y

-- | Internal
newTurtle x = newBreed "turtles" x

-- | Internal
newBreed b x = MkTurtle <$>
  newTVar x <*>
  newTVar b <*>
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

turtles :: CIO [AgentRef]
turtles = do
  (_,tw,_, _, _) <- ask
  (MkWorld _ ts) <- lift $ readTVarIO tw
  return $ IM.foldrWithKey (\ k x ks -> TurtleRef k x: ks) [] ts

patch_here :: CIO [AgentRef]
patch_here = do
  (_,_,TurtleRef _ (MkTurtle {xcor_ = x, ycor_ = y}), _, _) <- ask
  x' <- lift $ readTVarIO x
  y' <- lift $ readTVarIO y
  patch x' y'

turtle :: Int -> CIO [AgentRef]
turtle n = do
  (_, tw,_, _, _) <- ask
  (MkWorld _ ts) <- lift $ readTVarIO tw
  return $ [TurtleRef n (ts IM.! n)]


turtle_set :: [CIO [AgentRef]] -> CIO [AgentRef]
turtle_set ts = sequence ts >>= return . concat

patch_set = turtle_set

can_movep :: Double -> CIO Bool
can_movep n = patch_ahead n >>= \ p -> return (p /= [Nobody])

patch_ahead ::Double -> CIO [AgentRef]
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

dx :: CIO Double
dx = liftM sin_ heading

dy :: CIO Double
dy = liftM cos_ heading

heading :: CIO Double
heading = do
  (_,_,TurtleRef _ (MkTurtle {heading_ = h}), _, _) <- ask
  lift $ readTVarIO h

xcor :: CIO Double
xcor = do
  (_,_,TurtleRef _ (MkTurtle {xcor_ = x}), _, _) <- ask
  lift $ readTVarIO x

ycor :: CIO Double
ycor = do
  (_,_,TurtleRef _ (MkTurtle {ycor_ = y}), _, _) <- ask
  lift $ readTVarIO y

breed :: CIO String
breed = do
  (_,_,TurtleRef _ (MkTurtle {breed_ = b}), _, _) <- ask
  lift $ readTVarIO b

who :: CIO Int
who = do
  (_,_,TurtleRef i _, _, _) <- ask
  return i


random_xcor :: CIO Double
random_xcor = lift $ getStdRandom $ randomR ((fromIntegral $ min_pxcor_ conf),(fromIntegral $ max_pxcor_ conf))

random_ycor :: CIO Double
random_ycor = lift $ getStdRandom $ randomR ((fromIntegral $ min_pycor_ conf),(fromIntegral $ max_pycor_ conf))

random_pxcor :: CIO Int
random_pxcor = lift $ getStdRandom $ randomR (min_pxcor_ conf, max_pxcor_ conf)

random_pycor :: CIO Int
random_pycor = lift $ getStdRandom $ randomR (min_pycor_ conf, max_pycor_ conf)

random :: Int -> CIO Int
random x | x == 0 = return 0
         | x < 0 = lift $ getStdRandom $ randomR (x,0)
         | x > 0 = lift $ getStdRandom $ randomR (0,x)

random_float :: Double -> CIO Double
random_float x | x == 0 = return 0
               | x < 0 = lift $ getStdRandom $ randomR (x,0)
               | x > 0 = lift $ getStdRandom $ randomR (0,x)

-- | Internal
random_primary_color :: CIO Double
random_primary_color = do
  i <- lift $ randomRIO (0,13)
  return $ primary_colors !! i


-- | Reports a number suitable for seeding the random number generator. 
-- | todo
new_seed = undefined

-- | Sets the seed of the pseudo-random number generator to the integer part of number.
random_seed n = setStdGen $ mkStdGen n

-- | random-exponential reports an exponentially distributed random floating point number. 
-- | todo
random_exponential m = undefined

-- | random-gamma reports a gamma-distributed random floating point number as controlled by the floating point alpha and lambda parameters. 
-- | todo
random_gamma a l = undefined

-- | random-normal reports a normally distributed random floating point number. 
-- | todo
random_normal m s = undefined

-- | random-poisson reports a Poisson-distributed random integer. 
-- | todo
random_poisson m = undefined


-- | Reports an agentset containing all the turtles that are on the given patch or patches, or standing on the same patch as the given turtle or turtles. 
turtles_on :: [AgentRef] -> CIO [AgentRef]
turtles_on [] = return []
turtles_on ps@(PatchRef _ _ : _) = do
  with (liftM (flip elem ps . head) patch_here) =<< turtles
turtles_on ts@(TurtleRef _ _ : _) = do
  turtles_on =<< of_ (liftM head patch_here) ts

right :: Double -> CIO Double
right n = do
  h <- heading
  return $ mod_ (h + n) 360

left :: Double -> CIO Double
left n = do
  right (-n)


distance :: [AgentRef] -> CIO Double
distance [PatchRef (x,y) _] = do
  distancexy (fromIntegral x) (fromIntegral y)
distance [TurtleRef _ (MkTurtle {xcor_ = tx, ycor_ = ty})] = do
  x <- lift $ readTVarIO tx
  y <- lift $ readTVarIO ty
  distancexy x y

distancexy :: Double -> Double -> CIO Double
distancexy x' y' = do
  (_,_,ref,_,_) <- ask
  (x,y) <- case ref of
            PatchRef (x,y) _ -> return (fromIntegral x, fromIntegral y)
            TurtleRef _ (MkTurtle {xcor_ = tx, ycor_ = ty}) -> liftM2 (,) (lift $ readTVarIO tx) (lift $ readTVarIO ty)
  return $ sqrt ((delta x x' (fromIntegral $ max_pxcor_ conf)) ^ 2 + (delta y y' (fromIntegral $ max_pycor_ conf) ^ 2))
      where
        delta a1 a2 aboundary = min (abs (a2 - a1)) (abs (a2 + a1) + 1)



-- | todo
downhill = undefined

-- | todo
downhill4 = undefined

-- | todo
face = undefined

-- | todo
towards = undefined

-- | todo
towardsxy = undefined

in_radius :: [AgentRef] -> Double -> CIO [AgentRef]
in_radius as n = do
  (_,_,ref,_,_) <- ask
  (x, y) <- case ref of
    PatchRef (x,y) _ -> return $ (fromIntegral x, fromIntegral y)
    TurtleRef _ (MkTurtle {xcor_ = tx, ycor_ = ty}) -> liftM2 (,) (lift $ readTVarIO tx) (lift $ readTVarIO ty)
  with (distancexy x y >>= \ d -> return $ d <= n) as

in_cone = undefined

no_turtles :: CIO [AgentRef]
no_turtles = return []

no_patches :: CIO [AgentRef]
no_patches = return []

-- | Runs the given commands only if it's been more than number seconds since the last time this agent ran them in this context. Otherwise, the commands are skipped. 
every :: Double -> CIO a -> CIO ()
every n a = a >> wait n

-- | Wait the given number of seconds. (This needn't be an integer; you can specify fractions of seconds.) Note that you can't expect complete precision; the agent will never wait less than the given amount, but might wait slightly more. 
wait n = lift $ threadDelay (round $ n * 1000000)

max_pxcor :: CIO Int
max_pxcor = return $ max_pxcor_ conf

max_pycor :: CIO Int
max_pycor = return $ max_pycor_ conf

min_pxcor :: CIO Int
min_pxcor = return $ min_pxcor_ conf

min_pycor :: CIO Int
min_pycor = return $ min_pycor_ conf

world_width :: CIO Int
world_width = return $ (max_pxcor_ conf) - (min_pxcor_ conf) + 1

world_height :: CIO Int
world_height = return $ (max_pycor_ conf) - (min_pycor_ conf) + 1

ticks :: CIO Double
ticks = do
  (gs, _, _, _, _) <- ask
  lift $ readTVarIO (gs ! 1)


one_of :: [a] -> CIO a
one_of [] = error "empty list"
one_of l = do
  v <- lift $ randomRIO (0, length l)
  return (l !! v)

-- | todo optimize with arrays <http://www.haskell.org/haskellwiki/Random_shuffle>
shuffle :: Eq a => [a] -> CIO [a]
shuffle [] = return []
shuffle l = shuffle' l (length l) where
    shuffle [x] 1 = return [x]
    shuffle' l i = do
      x <- one_of l
      xs <- shuffle' (delete x l) (i-1)
      return $ x:xs


show_ :: Show a => a -> CIO ()
show_ a = do
  (_,_, r, _, _) <- ask
  lift $ putStrLn $ (case r of
                           ObserverRef -> "observer: "
                           PatchRef (x,y) _ -> "(patch " ++ show x ++ " " ++ show y ++ "): "
                           TurtleRef i _ -> "(turtle " ++ show i ++ "): ")   ++ show a


print_ :: Show a => a -> CIO ()
print_ a = do
  lift $ putStrLn $ show a


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

