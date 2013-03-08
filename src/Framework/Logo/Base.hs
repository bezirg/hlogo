module Framework.Logo.Base where

import Control.Concurrent.STM
import Control.Monad.Reader
import qualified Data.IntMap as IM
import qualified Data.Map as M
import Data.Array
import System.Random (StdGen)

data PenMode = Down | Up | Erase

data Turtle = MkTurtle {
      who_ :: TVar Int,
      breed_ :: TVar String,
      color_ :: TVar Double,
      heading_ :: TVar Double,
      xcor_ :: TVar Double,
      ycor_ :: TVar Double,
      shape_ :: TVar String,
      label_ :: TVar String,
      label_color :: TVar Double,
      hiddenp_ :: TVar Bool,
      size_ :: TVar Double,
      pen_size_ :: TVar Double,
      pen_mode_ :: TVar PenMode
    }
              deriving (Eq)

data Patch = MkPatch {
      pxcor_ :: TVar Int,
      pycor_ :: TVar Int,
      pcolor_ :: TVar Double,
      plabel_ :: TVar String,
      plabel_color_ :: TVar Double
      }
           deriving (Eq)

type Patches = M.Map (Int, Int) Patch

type Turtles = IM.IntMap Turtle

-- static type to int
type Globals = Array Int (TVar Int)

data World = MkWorld Patches Turtles

data AgentRef = PatchRef (Int,Int) Patch
              | TurtleRef Int Turtle
              | ObserverRef
              | Nobody
                deriving (Eq)

type Context = (Globals, TVar World, AgentRef, TChan String, TVar StdGen)

type CIO a = ReaderT Context IO a

type CSTM a = ReaderT Context STM a
