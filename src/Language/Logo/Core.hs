{-# LANGUAGE CPP #-}
{-# OPTIONS_HADDOCK show-extensions #-}
-- | 
-- Module      :  Language.Logo.Core
-- Copyright   :  (c) 2013-2016, the HLogo team
-- License     :  BSD3
-- Maintainer  :  Nikolaos Bezirgiannis <bezirgia@cwi.nl>
-- Stability   :  experimental
--
-- The core long-lived components of the simulation engine
module Language.Logo.Core
    ( cInit
    , __tick
    , __who
    , __timer 
    , __patches
    , __turtles
    , __links
    , __printQueue
    ) where

import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import Language.Logo.Base
import Language.Logo.CmdOpt
import qualified Data.Map.Strict as M (empty)
import qualified  Data.IntMap.Strict as IM (empty)
import qualified Data.Vector as V (fromList, replicateM)
import Data.Time.Clock (UTCTime,getCurrentTime)
import Control.Monad
import System.Random.TF.Gen (TFGen,seedTFGen)
import System.IO.Unsafe (unsafePerformIO)
import Data.IORef (IORef, newIORef)
#if __GLASGOW_HASKELL__ < 710
import Control.Applicative ((<$>),(<*>))
#endif

{-# NOINLINE __tick #-}
-- | The global (atomically-modifiable) tick variable
--
-- Double because NetLogo also allows different than 1-tick increments
__tick :: IORef Double
__tick = unsafePerformIO $ newIORef (error "The tick counter has not been started yet. Use RESET-TICKS.")

{-# NOINLINE __who #-}
-- | The global (atomically-modifiable) who-counter variable
__who :: TVar Int
__who = unsafePerformIO $ newTVarIO 0

{-# NOINLINE __timer #-}
-- | The global (atomically-modifiable) timer variable
__timer :: TVar UTCTime
__timer = unsafePerformIO $ newTVarIO undefined

{-# NOINLINE __patches #-}
-- | The global turtles vector
__patches :: Patches
__patches = V.fromList [unsafePerformIO (newPatch x y) 
                       | x <- [min_pxcor_ cmdOpt..max_pxcor_ cmdOpt]
                       , y <- [min_pycor_ cmdOpt..max_pycor_ cmdOpt] 
                       ]
-- two other ways

-- __patches = unsafePerformIO $ let b = max_pycor_ conf - min_pycor_ conf + 1
--                               in V.generateM ((max_pxcor_ conf-min_pxcor_ conf+1)*b)
--                                      (\ i ->
--                                           let 
--                                               (q,r) = i `quotRem` b
--                                           in newPatch (q+min_pxcor_ conf) (r+min_pycor_ conf)
--                                      )

-- __patches = V.unfoldr (\ (x,y) -> if y > max_pycor_ conf
--                                  then
--                                      if x == max_pxcor_ conf
--                                      then Nothing
--                                      else Just (unsafePerformIO (newPatch (x+1) (min_pycor_ conf)), (x+1,min_pycor_ conf+1))
--                                  else Just (unsafePerformIO (newPatch x y), (x, y+1))
--                       ) (min_pxcor_ conf, min_pycor_ conf)


{-# INLINE newPatch #-}
-- | Returns a 'Patch' structure with default arguments (based on NetLogo)
newPatch :: Int -> Int -> IO Patch
newPatch x y = let po = 1       -- patches_own only one element for now
               in MkPatch x y <$>
               newTVarIO 0 <*>
               newTVarIO "" <*>
               newTVarIO 9.9 <*>
               -- init the patches-own variables to 0
               V.replicateM po (newTVarIO 0)

{-# NOINLINE __turtles #-}
__turtles :: TVar Turtles
__turtles = unsafePerformIO $ newTVarIO IM.empty

{-# NOINLINE __links #-}
__links :: TVar Links
__links = unsafePerformIO $ newTVarIO M.empty

{-# NOINLINE __printQueue #-}
__printQueue :: TQueue String
__printQueue = unsafePerformIO $ newTQueueIO


-- | Reads the Configuration, initializes globals to 0, spawns the Patches, and forks the IO Printer.
-- Takes the length of the patch var from TH (trick) for the patches own array.
-- Returns the top-level Observer context.
cInit :: Int -> IO (Observer,a,IORef TFGen)
cInit po = do

  -- The printer just reads an IO chan for incoming text and outputs it to standard output.
  _ <- forkIO $ forever $ putStrLn =<< atomically (readTQueue __printQueue) 

  (atomically . writeTVar __timer) =<< getCurrentTime

  ogen <- newIORef (seedTFGen (40, 0, 0, 0))   -- default StdGen seed equals 0

  return (undefined,undefined,ogen)             -- the initial context



