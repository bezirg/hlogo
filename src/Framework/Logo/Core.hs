module Framework.Logo.Core (
                            cInit
) where

import Control.Concurrent (forkIO)
import Control.Monad (forever)
import Control.Applicative
import Control.Monad.Reader (lift)
import Control.Concurrent.STM
import Framework.Logo.Conf
import Framework.Logo.Base
import Framework.Logo.Prim
import qualified Data.Map as M (fromAscList, empty)
import qualified  Data.IntMap as IM (empty)
import Data.Array (listArray)
import Control.Monad
import System.Random (mkStdGen)

-- | Reads the Configuration, initializes globals to 0, spawns the Patches, and forks the IO Printer.
-- Takes the length of the global from TemplateHaskell (trick) to determine the size of the globals array
-- the length of the patch var from TH (trick) for the patches own array.
-- Returns the top-level Observer context.
cInit :: Int -> Int -> IO Context
cInit gl po = do
  -- read dimensions from conf
  let mx = max_pxcor_ conf
  let my = max_pycor_ conf
  -- initialize globals
  gs <- return . listArray (0, fromIntegral gl+1) =<< replicateM (fromIntegral gl + 2) (newTVarIO 0)
  -- spawn patches
  ps <- sequence [do
                   p <- newPatch x y po
                   return ((x, y), p)
                 | x <- [-mx..mx], y <- [-my..my]]
  -- initialize
  let ts = IM.empty
  let ls = M.empty
  tw <- newTVarIO (MkWorld (M.fromAscList ps) ts ls)
  tp <- newTChanIO
  ts <- newTVarIO (mkStdGen 0)   -- default StdGen seed equals 0
  forkIO $ printer tp
  return (gs, tw, ObserverRef, tp, ts)

-- | The printer just reads an IO chan for incoming text and outputs it to standard output.
printer:: TChan String -> IO ()
printer tp = forever $ do
  v <- atomically $ readTChan tp
  putStrLn v


-- | Returns a 'Patch' structure with default arguments (based on NetLogo)
newPatch :: Int -> Int -> Int -> IO Patch
newPatch x y po = MkPatch <$>
                  return x <*>
                  return y <*>
                  newTVarIO 0 <*>
                  newTVarIO "" <*>
                  newTVarIO 9.9 <*>
                  -- init the patches-own variables to 0
                  (return . listArray (0, fromIntegral po -1) =<< replicateM (fromIntegral po) (newTVarIO 0))
