module Framework.Logo.Core where

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

cInit :: IO Context
cInit = do
  let mx = max_pxcor_ conf
  let my = max_pycor_ conf
  gs <- return . listArray (0,9) =<< replicateM 10 (newTVarIO 0)
  ps <- sequence [do
                   p <- newPatch x y
                   return ((x, y), p)
                 | x <- [-mx..mx], y <- [-my..my]]
  let ts = IM.empty
  tw <- newTVarIO (MkWorld (M.fromAscList ps) ts)
  tp <- newTChanIO
  ts <- newTVarIO (mkStdGen 0)
  forkIO $ printer tp
  return (gs, tw, ObserverRef, tp, ts)

printer:: TChan String -> IO ()
printer tp = forever $ do
  v <- atomically $ readTChan tp
  putStrLn v


newPatch x y =liftM5 MkPatch (newTVarIO x) (newTVarIO y) (newTVarIO 0) (newTVarIO "") (newTVarIO 9.9)
