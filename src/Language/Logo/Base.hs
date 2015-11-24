{-# LANGUAGE CPP, TypeFamilies, EmptyDataDecls #-}
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
import Data.Array (Array)
import Data.Vector (Vector)
import System.Random (StdGen, mkStdGen)
import System.IO.Unsafe (unsafePerformIO)
#ifdef STATS_STM
import Data.IORef
#endif

class Show s => Player s where
    gen_ :: s -> TVar StdGen

-- | NB: Eq needed for agentset operations (because it is list for now) 
class (Eq s, Player s) => Agent s where      
    --type AgentSet s
    ask :: C s p IO _b -> [s] -> C p p' IO ()
    of_ :: C s p IO b -> [s] -> C p p' IO [b]

    
class Agent s => TurtleLink s where
    breed_ :: s -> TVar String
    shape_ :: s -> TVar String
    label_ :: s -> TVar String
    label_color_ :: s -> TVar Double
    color_ :: s -> TVar Double
    die :: C s _s' STM ()

data Observer

-- | Following the NetLogo convention, PenMode is an Algebraic Data Type (ADT)
data PenMode = Down | Up | Erase

-- | The 'Turtle' datatype is a record with each field being a transactional variable (TVar) holding
-- an attribute value of 'Turtle'.
-- For now only the default turtle attributes are supported.
data Turtle = MkTurtle {
      who_ :: !Int               -- on creation
    , tbreed_ :: TVar String          -- on creation
    , tcolor_ :: TVar Double
    , heading_ :: TVar Double
    , xcor_ :: TVar Double
    , ycor_ :: TVar Double
    , tshape_ :: TVar String
    , tlabel_ :: TVar String
    , tlabel_color_ :: TVar Double
    , hiddenp_ :: TVar Bool
    , size_ :: TVar Double
    , pen_size_ :: TVar Double
    , pen_mode_ :: TVar PenMode
    , tvars_ :: Array Int (TVar Double)
    , tgen_ :: TVar StdGen
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
    , pgen_ :: TVar StdGen
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
    , lbreed_ :: TVar String
    , thickness_ :: TVar Double
    , lshape_ :: TVar String
    , tie_mode :: TVar TieMode
    , lvars_ :: Array Int (TVar Double)
    , lgen_ :: TVar StdGen
#ifdef STATS_STM
    , ltotalstm :: IORef Int
    , lsuccstm :: IORef Int
#endif
    } deriving (Eq)

data TieMode = None | Fixed

-- | The 'Patches' ADT is an ordered map (dictionary) from coordinates (Int, Int) to 'Patch' data structures
type Patches = Vector (Vector Patch)

-- | The 'Turtles' ADT is an 'IM.IntMap' from who indices to 'Turtle' data structures
type Turtles = IM.IntMap Turtle

-- type Turtles_ = IOVector Turtle

-- | The 'Links' ADT is an ordered map (dictionary) from turtle Int indices (from, to) to 'Link' data structures
type Links = M.Map (Int, Int) Link

type C s s' m a = ReaderT (s,s') m a

instance Ord Turtle where
    compare (MkTurtle {who_ = w1}) (MkTurtle {who_ = w2}) = compare w1 w2

instance Ord Link where
    compare (MkLink {end1_ = xe1, end2_ = xe2}) (MkLink {end1_ = ye1, end2_ = ye2}) = compare (xe1,xe2) (ye1,ye2)
    
instance Show Turtle where
    show (MkTurtle {who_ = tw}) = "(turtle " ++ show tw ++ ")"

instance Show Patch where
    show (MkPatch {pxcor_ = px, pycor_ = py}) = "(patch " ++ show px ++ " " ++ show py ++ ")"

instance Show Link where
    show (MkLink {end1_ = e1, end2_ = e2}) = "(link " ++ show e1 ++ " " ++ show e2 ++ ")"

instance Show Observer where
    show _ = "observer"

instance Player Turtle where
    gen_ = tgen_

instance Player Patch where
    gen_ = pgen_

instance Player Link where
    gen_ = lgen_

instance Player Observer where
    gen_ _ = ogen_

{-# NOINLINE ogen_ #-}
ogen_ :: TVar StdGen
ogen_ = unsafePerformIO $ newTVarIO (mkStdGen 0)   -- default StdGen seed equals 0
