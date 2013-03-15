module Framework.Logo.Core (
                            cInit
) where

import Control.Concurrent (forkIO)
import Control.Monad (forever)
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
-- Takes the length of the global from TemplateHaskell (trick), to determine the size of the globals array
-- Returns the top-level Observer context.
cInit :: Integer -> IO Context
cInit gl = do
  -- read dimensions from conf
  let mx = max_pxcor_ conf
  let my = max_pycor_ conf
  -- initialize globals
  gs <- return . listArray (0, fromIntegral gl+1) =<< replicateM (fromIntegral gl + 2) (newTVarIO 0)
  -- spawn patches
  ps <- sequence [do
                   p <- newPatch x y
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
newPatch :: Int -> Int -> IO Patch
newPatch x y =liftM5 MkPatch (return x) (return y) (newTVarIO 0) (newTVarIO "") (newTVarIO 9.9)
