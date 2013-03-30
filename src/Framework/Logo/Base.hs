{-# LANGUAGE DeriveDataTypeable #-}
-- | The module contains the Base datatypes of the Framework.
module Framework.Logo.Base where

import Control.Concurrent.STM
import Control.Monad.Trans.Reader
import qualified Data.IntMap as IM
import qualified Data.Map as M
import Data.Array
import Data.Typeable
import System.Random (StdGen)

-- | Following the NetLogo convention, PenMode is an Algebraic Data Type (ADT)
data PenMode = Down | Up | Erase

-- | The 'Turtle' datatype is a record with each field being a transactional variable (TVar) holding
-- an attribute value of 'Turtle'.
-- For now only the default turtle attributes are supported.
data Turtle = MkTurtle {
      who_ :: Int,               -- on creation
      breed_ :: TVar String,          -- on creation
      color_ :: TVar Double,
      heading_ :: TVar Double,
      xcor_ :: TVar Double,
      ycor_ :: TVar Double,
      shape_ :: TVar String,
      label_ :: TVar String,
      label_color_ :: TVar Double,
      hiddenp_ :: TVar Bool,
      size_ :: TVar Double,
      pen_size_ :: TVar Double,
      pen_mode_ :: TVar PenMode,
      tvars_ :: Array Int (TVar Double)
    }
              deriving (Eq)

-- | The 'Patch' datatype follows a similar philosophy with the 'Turtle' (ADT).
-- Each field is a transactional variable (TVar) storing an attribute value of 'Patch'
-- For now only the default patch attributes are supported.
data Patch = MkPatch {
      pxcor_ :: Int,             -- on creation
      pycor_ :: Int,             -- on creation
      pcolor_ :: TVar Double,
      plabel_ :: TVar String,
      plabel_color_ :: TVar Double,
      pvars_ :: Array Int (TVar Double)
      }
           deriving (Eq)

data Link = MkLink {
      end1_ :: Int,              -- on creation
      end2_ :: Int,              -- on creation
      directed_ :: Bool,          -- on creation
      lcolor_ :: TVar Double,
      llabel_ :: TVar String,
      llabel_color_ :: TVar Double,
      lhiddenp_ :: TVar Bool,
      lbreed_ :: String,
      thickness_ :: TVar Double,
      lshape_ :: TVar String,
      tie_mode :: TVar TieMode,
      lvars_ :: Array Int (TVar Double)
    }
          deriving (Eq)

data TieMode = None | Fixed

-- | The 'Patches' ADT is an ordered map (dictionary) from coordinates (Int, Int) to 'Patch' data structures
type Patches = M.Map (Int, Int) Patch

-- | The 'Turtles' ADT is an 'IM.IntMap' from who indices to 'Turtle' data structures
type Turtles = IM.IntMap Turtle

-- | The 'Links' ADT is an ordered map (dictionary) from turtle Int indices (from, to) to 'Link' data structures
type Links = M.Map (Int, Int) Link

-- | The 'Globals' structure is an array of Int-indices pointing to Double (for now) variables.
-- Index 0 is reserved for holding the global who variable.
-- Index 1 is reserved for holding the global ticks variable.
-- The globals variables start indexing from index 2.
-- In the future it must be polymorphic on the container type.
type Globals = Array Int (TVar Double)

-- | The 'World' is the union of 'Patches' and 'Turtles'
data World = MkWorld Patches Turtles Links

-- | An 'AgentRef' is a reference to an agent of the framework.
data AgentRef = PatchRef (Int,Int) Patch
              | TurtleRef Int Turtle
              | LinkRef (Int,Int) Link
              | ObserverRef     -- ^ 'ObserverRef' is needed to restrict the context of specific built-in functions. 
              | Nobody          -- ^ 'Nobody' is the null reference in NetLogo.
                deriving (Eq, Ord, Typeable)

-- | The 'Context' datatype is a tuple of the global variables, the current agents of the 'World' (through a transactional variable), a caller reference 'AgentRef', a safe String-channel for Input/Output, the current random seed in a TVar and the CallerRef (myself)
type Context = (Globals, TVar World, AgentRef, TChan String, TVar StdGen, AgentRef)

type C m a = ReaderT Context m a

-- | The enhanced STM monad having a particular calling context
type CSTM a = ReaderT Context STM a

-- | The enhanced IO monad having a particular calling context
type CIO a = ReaderT Context IO a


instance Ord Turtle where
    compare (MkTurtle {who_ = w1}) (MkTurtle {who_ = w2}) = compare w1 w2

instance Ord Patch where
    compare (MkPatch {pxcor_ = x1, pycor_ = y1}) (MkPatch {pxcor_ = x2, pycor_ = y2}) = compare (x1,y1) (x2,y2)

instance Ord Link where
    compare (MkLink {end1_ = xe1, end2_ = xe2}) (MkLink {end1_ = ye1, end2_ = ye2}) = compare (xe1,xe2) (ye1,ye2)
    
instance Show AgentRef where
    show (PatchRef (x,y) _) = "PatchRef (" ++ show x ++ "," ++ show y ++ ")"
    show (TurtleRef w _) = "TurtleRef " ++ show w
    show (LinkRef (x,y) _) = "LinkRef (" ++ show x ++ "," ++ show y ++ ")"
    show (ObserverRef) = "ObserverRef"
    show Nobody = "nobody"
