import qualified Control.Concurrent.Thread as Thread
import Control.Concurrent.STM
import Control.Applicative
import Data.Array
import Control.Monad (replicateM_, replicateM)

-- | Following the NetLogo convention, PenMode is an Algebraic Data Type (ADT)
data PenMode = Down | Up | Erase

main = do
  ts <- atomically $ replicateM 100000 newTurtle -- atomically $ create_turtles 100000
  (_, wait1) <- Thread.forkIO $ mapM_ fdTurtles ts
  (_, wait2) <- Thread.forkIO $ mapM_ incTurtles ts
  print =<< Thread.result =<< wait1
  print =<< Thread.result =<< wait2
  return ()

fdTurtles t = atomically $ do 
                modifyTVar' ( t ! 0) (+1)
                modifyTVar' ( t ! 1) (+1)
              >>
              return "fd"
incTurtles t = atomically $ do 
                     modifyTVar' (t ! 3) (+1)
                     modifyTVar' (t ! 4) (+1)
                  
               >> 
               return "inc"

newTurtle = (return . listArray (0, 12) =<< replicateM (13) (newTVar 0))

