{-# LANGUAGE CPP #-}
{-# OPTIONS_HADDOCK show-extensions #-}
-- | 
-- Module      :  Language.Logo.Base
-- Copyright   :  (c) 2013-2015, the HLogo team
-- License     :  BSD3
-- Maintainer  :  Nikolaos Bezirgiannis <bezirgia@cwi.nl>
-- Stability   :  experimental
--
-- The module contains the Base datatypes of the Language.
module Language.Logo.Base where

import Control.Concurrent.STM
import Control.Monad.Trans.Reader
import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict as M
import Data.Array
-- import Data.Vector.Mutable (IOVector)
import Data.Typeable
import System.Random (StdGen)
#ifdef STATS_STM
import Data.IORef
#endif

-- | Following the NetLogo convention, PenMode is an Algebraic Data Type (ADT)
data PenMode = Down | Up | Erase

-- | The 'Turtle' datatype is a record with each field being a transactional variable (TVar) holding
-- an attribute value of 'Turtle'.
-- For now only the default turtle attributes are supported.
data Turtle = MkTurtle {
      who_ :: !Int               -- on creation
    , breed_ :: TVar String          -- on creation
    , color_ :: TVar Double
    , heading_ :: TVar Double
    , xcor_ :: TVar Double
    , ycor_ :: TVar Double
    , shape_ :: TVar String
    , label_ :: TVar String
    , label_color_ :: TVar Double
    , hiddenp_ :: TVar Bool
    , size_ :: TVar Double
    , pen_size_ :: TVar Double
    , pen_mode_ :: TVar PenMode
    , tvars_ :: Array Int (TVar Double)
    , tgen :: TVar StdGen
    , init_xcor_ :: !Int
    , init_ycor_ :: !Int
#ifdef STATS_STM
    , ttotalstm :: IORef Int
    , tsuccstm :: IORef Int
#endif
    } deriving (Eq)

-- | The 'Patch' datatype follows a similar philosophy with the 'Turtle' (ADT).
-- Each field is a transactional variable (TVar) storing an attribute value of 'Patch'
-- For now only the default patch attributes are supported.
data Patch = MkPatch {
      pxcor_ :: !Int             -- on creation
    , pycor_ :: !Int             -- on creation
    , pcolor_ :: TVar Double
    , plabel_ :: TVar String
    , plabel_color_ :: TVar Double
    , pvars_ :: Array Int (TVar Double)
    , pgen :: TVar StdGen
#ifdef STATS_STM
    , ptotalstm :: IORef Int
    , psuccstm :: IORef Int
#endif
      } deriving (Eq)

data Link = MkLink {
      end1_ :: !Int              -- on creation
    , end2_ :: !Int              -- on creation
    , directed_ :: Bool          -- on creation
    , lcolor_ :: TVar Double
    , llabel_ :: TVar String
    , llabel_color_ :: TVar Double
    , lhiddenp_ :: TVar Bool
    , lbreed_ :: String
    , thickness_ :: TVar Double
    , lshape_ :: TVar String
    , tie_mode :: TVar TieMode
    , lvars_ :: Array Int (TVar Double)
    , lgen :: TVar StdGen
#ifdef STATS_STM
    , ltotalstm :: IORef Int
    , lsuccstm :: IORef Int
#endif
    } deriving (Eq)

data TieMode = None | Fixed

-- | The 'Patches' ADT is an ordered map (dictionary) from coordinates (Int, Int) to 'Patch' data structures
type Patches = Array (Int,Int) Patch

-- | The 'Turtles' ADT is an 'IM.IntMap' from who indices to 'Turtle' data structures
type Turtles = IM.IntMap Turtle

-- type Turtles_ = IOVector Turtle

-- | The 'Links' ADT is an ordered map (dictionary) from turtle Int indices (from, to) to 'Link' data structures
type Links = M.Map (Int, Int) Link

-- | The 'World' is the union of 'Patches' and 'Turtles'
data World = MkWorld Turtles Links

-- | An 'AgentRef' is a reference to an agent of the framework.
data AgentRef = PatchRef !(Int,Int) Patch
              | TurtleRef !Int Turtle
              | LinkRef !(Int,Int) Link
              | ObserverRef (TVar StdGen)     -- ^ 'ObserverRef' is needed to restrict the context of specific built-in functions.  It carries its random generator. It should not be a first-class citizen (returned as an AgentRef)
              | Nobody          -- ^ 'Nobody' is the null reference in NetLogo.
                deriving (Eq, Typeable)

-- | The 'Context' datatype is a tuple the current agents of the 'World' (through a transactional variable), a caller reference 'AgentRef', a safe String-channel for Input/Output  and the CallerRef (myself)
type Context = (TVar World
               , AgentRef       -- self
               , TQueue String
               , AgentRef)      -- myself (the caller only through ask/of-like, i.e. not all callers should be returned)

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
    show (ObserverRef _) = "ObserverRef"
    show Nobody = "nobody"

instance Ord AgentRef where
    (TurtleRef w1 _) `compare` (TurtleRef w2 _) = compare w1 w2
    (PatchRef (x1,y1) _) `compare` (PatchRef (x2,y2) _) = let c1 = compare x1 x2
                                                          in if c1 == EQ
                                                             then compare y2 y1
                                                             else c1
    (LinkRef (x1,y1) _) `compare` (LinkRef (x2,y2) _) = compare (x1,y1) (x2,y2)
    _ `compare` _ = error "Not comparable, because not the same type of agent"
