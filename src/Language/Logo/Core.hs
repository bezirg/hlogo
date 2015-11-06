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
) where

import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import Language.Logo.Conf
import Language.Logo.Base
import qualified Data.Map as M (fromAscList, empty)
import qualified  Data.IntMap as IM (empty)
import Data.Array (listArray)
import Data.Time.Clock (UTCTime,getCurrentTime)
import Control.Monad
import System.Random (mkStdGen)
import System.IO.Unsafe (unsafePerformIO)
#ifdef STATS_STM
import Data.IORef (newIORef)
#endif
#if __GLASGOW_HASKELL__ < 710
import Control.Applicative
#endif

{-# NOINLINE __tick #-}
-- | The global (atomically-modifiable) tick variable
--
-- Double because NetLogo also allows different than 1-tick increments
__tick :: TVar Double
__tick = unsafePerformIO $ newTVarIO undefined

{-# NOINLINE __who #-}
-- | The global (atomically-modifiable) who-counter variable
__who :: TVar Int
__who = unsafePerformIO $ newTVarIO undefined

{-# NOINLINE __timer #-}
-- | The global (atomically-modifiable) timer variable
__timer :: TVar UTCTime
__timer = unsafePerformIO $ newTVarIO undefined

-- | Reads the Configuration, initializes globals to 0, spawns the Patches, and forks the IO Printer.
-- Takes the length of the patch var from TH (trick) for the patches own array.
-- Returns the top-level Observer context.
cInit :: Int -> IO Context
cInit po = do
  -- read dimensions from conf
  let max_x = max_pxcor_ conf
  let max_y = max_pycor_ conf
  let min_x = min_pxcor_ conf
  let min_y = min_pycor_ conf
  t <- getCurrentTime
  -- initialize globals
  atomically $ do
                writeTVar __tick 0
                writeTVar __who 0
                writeTVar __timer t
  -- spawn patches
  ps <- sequence [do
                   p <- newPatch x y
                   return ((x, y), p)
                 | x <- [min_x..max_x], y <- [min_y..max_y]]
  -- initialize
  let ts = IM.empty
  let ls = M.empty
  tw <- newTVarIO (MkWorld (M.fromAscList ps) ts ls)
  tp <- newTChanIO
  g <- newTVarIO (mkStdGen 0)   -- default StdGen seed equals 0
  forkIO $ printer tp
  return (tw, ObserverRef g, tp, Nobody)
  where
    -- | The printer just reads an IO chan for incoming text and outputs it to standard output.
    printer:: TChan String -> IO ()
    printer tp = forever $ do
                   v <- atomically $ readTChan tp
                   putStrLn v

    -- | Returns a 'Patch' structure with default arguments (based on NetLogo)
    newPatch :: Int -> Int -> IO Patch
    newPatch x y = MkPatch <$>
                  return x <*>
                  return y <*>
                  newTVarIO 0 <*>
                  newTVarIO "" <*>
                  newTVarIO 9.9 <*>
                  -- init the patches-own variables to 0
                  (return . listArray (0, fromIntegral po -1) =<< replicateM (fromIntegral po) (newTVarIO 0)) <*>
                  newTVarIO (mkStdGen (x + y * 1000))
#ifdef STATS_STM
                  <*> pure (unsafePerformIO (newIORef 0)) <*> pure (unsafePerformIO (newIORef 0))
#endif



