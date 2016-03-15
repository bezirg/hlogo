{-# LANGUAGE CPP #-}
{-# OPTIONS_HADDOCK show-extensions #-}
-- | 
-- Module      :  Language.Logo.Core
-- Copyright   :  (c) 2013-2015, the HLogo team
-- License     :  BSD3
-- Maintainer  :  Nikolaos Bezirgiannis <bezirgia@cwi.nl>
-- Stability   :  experimental
--
-- The core long-lived components of the simulation engine
module Language.Logo.Core (
                           cInit
                          ,__tick
                          ,__who
                          ,__timer 
                          ,__tg
                          ,__patches
                          ,__turtles
                          ,__links
                          ,__printQueue
                          ) where

import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import Language.Logo.Conf
import Language.Logo.Base
import qualified Data.Map.Strict as M (empty)
import qualified  Data.IntMap.Strict as IM (empty)
import qualified Data.Vector as V (fromList, replicateM)
import Data.Time.Clock (UTCTime,getCurrentTime)
import Control.Monad
import System.Random.TF.Gen (TFGen,seedTFGen)
import System.IO.Unsafe (unsafePerformIO)
import qualified Control.Concurrent.Thread.Group as ThreadG (ThreadGroup, new)
import Data.IORef (IORef, newIORef)
#if __GLASGOW_HASKELL__ < 710
import Control.Applicative
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

{-# NOINLINE __tg #-}
-- | The global group of running threads. Observer uses this ThreadGroup in 'ask', as a synchronization point to wait until all of them are finished.
__tg :: ThreadG.ThreadGroup
__tg = unsafePerformIO $ ThreadG.new

{-# NOINLINE __patches #-}
-- | The global turtles vector
__patches :: Patches
__patches = V.fromList [unsafePerformIO (newPatch x y) 
                       | x <- [min_pxcor_ conf..max_pxcor_ conf]
                       , y <- [min_pycor_ conf..max_pycor_ conf] 
                       ]

{-# INLINE newPatch #-}
-- | Returns a 'Patch' structure with default arguments (based on NetLogo)
newPatch :: Int -> Int -> IO Patch
newPatch x y = let po = 1       -- patches_own only one element for now
               in MkPatch x y <$>
               newTVarIO 0 <*>
               newTVarIO "" <*>
               newTVarIO 9.9 <*>
               -- init the patches-own variables to 0
               (V.replicateM po (newTVarIO 0))
#ifdef STATS_STM
               <*> pure (unsafePerformIO (newIORef 0)) <*> pure (unsafePerformIO (newIORef 0))
#endif

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
cInit :: Int -> IO (Observer,a,TVar TFGen)
cInit po = do
  _ <- forkIO $ printer

  t <- getCurrentTime
  atomically $ writeTVar __timer t

  ogen <- newTVarIO (seedTFGen (40, 0, 0, 0))   -- default StdGen seed equals 0

  return (undefined,undefined,ogen)             -- the initial context


  where
    -- | The printer just reads an IO chan for incoming text and outputs it to standard output.
    printer :: IO ()
    printer = forever $ putStrLn =<< atomically (readTQueue __printQueue)




