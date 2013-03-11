module Framework.Logo.Core (
                            cInit
) where

import Control.Concurrent (forkIO)
import Control.Monad (forever)
import Control.Concurrent.STM
import Framework.Logo.Conf
import Framework.Logo.Base
import Framework.Logo.Prim
import qualified Data.Map as M (fromAscList)
import qualified  Data.IntMap as IM (empty)
import Data.Array (listArray)
import Control.Monad
import System.Random (mkStdGen)

-- | Reads the Configuration, initializes globals to 0, spawns the Patches, and forks the IO Printer.
-- Returns the top-level Observer context.
cInit :: IO Context
cInit = do
  -- read dimensions from conf
  let mx = max_pxcor_ conf
  let my = max_pycor_ conf
  -- initialize globals
  gs <- return . listArray (0,9) =<< replicateM 10 (newTVarIO 0)
  -- spawn patches
  ps <- sequence [do
                   p <- newPatch x y
                   return ((x, y), p)
                 | x <- [-mx..mx], y <- [-my..my]]
  -- initialize
  let ts = IM.empty
  tw <- newTVarIO (MkWorld (M.fromAscList ps) ts)
  tp <- newTChanIO
  ts <- newTVarIO (mkStdGen 0)
  forkIO $ printer tp
  return (gs, tw, ObserverRef, tp, ts)

-- | The printer just reads an IO chan for incoming text and outputs it to standard output.
printer:: TChan String -> IO ()
printer tp = forever $ do
  v <- atomically $ readTChan tp
  putStrLn v


-- | Returns a 'Patch' structure with default arguments (based on NetLogo)
newPatch :: Int -> Int -> IO Patch
newPatch x y =liftM5 MkPatch (newTVarIO x) (newTVarIO y) (newTVarIO 0) (newTVarIO "") (newTVarIO 9.9)
