{-# LANGUAGE CPP, EmptyDataDecls, TypeFamilies #-}
{-# OPTIONS_HADDOCK show-extensions #-}
-- | 
-- Module      :  Language.Logo.Base
-- Copyright   :  (c) 2013-2015, the HLogo team
-- License     :  BSD3
-- Maintainer  :  Nikolaos Bezirgiannis <bezirgia@cwi.nl>
-- Stability   :  experimental
--
-- The module contains the Base datatypes of the Language.
module Language.Logo.Base (
                          -- * The typeclasses (interfaces) of each participant in a simulation
                          Agent (..), With (..), TurtleLink (..), C
                          -- * The Observer datastructure
                          ,Observer
                          -- * The agents' datastructures holding the attributes of the agents 
                          ,Turtle (..), Patch (..), Link (..),PenMode (..), TieMode (..)
                          -- * The containers storing multiple agents
                          ,Patches,Turtles,Links
                          , One, Many
                          ) where

import Control.Concurrent.STM
import Control.Monad.Trans.Reader (ReaderT)
import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict as M
import Data.Array (Array)
import Data.Vector (Vector)
import System.Random.TF.Gen (TFGen)
#ifdef STATS_STM
import Data.IORef
#endif

-- | An agent (subtype of 'Player') can be 'ask'ed to do some work or return a value `of_` his. 
--
-- Agents (of the same type, since we are typed) can be checked for 'Eq'uality (= in NetLogo, == in HLogo), 'Ord'ered (>,<,>=...)
-- and 'Show'ed their identiy to screen.
class Agent s where      
    ask :: C (One s) p IO _b -> s -> C p p' IO ()
    of_ :: C (One s) p IO b -> s -> C p p' IO (Many s b)

class With s where
    with :: C (One s) p IO Bool -> s -> C p p' IO s

type family One a where
    One Patches = Patch
    One Turtles = Turtle
    One Links = Link
    One a = a

type family Many a b where
    Many Turtle b = b
    Many Patch b = b
    Many Link b = b
    Many a b = [b]
    
-- | A subtype of Agent is a TurtleLink, i.e. either a Turtle or a Link.
-- 
-- This class is needed since turtle/links have some common attribute names
-- and share certain primitives.
class Agent s => TurtleLink s where
    breed_ :: s -> TVar String
    shape_ :: s -> TVar String
    label_ :: s -> TVar String
    label_color_ :: s -> TVar Double
    color_ :: s -> TVar Double
    die :: C s _s' STM ()

-- | The observer is an empty datatype. It cannot be constructed (inhabited by any value),
-- and is only there to restrict the 'self'&'myself' reader 'C'ontext.
data Observer

-- | The 'Turtle' datatype is a record with each field being a transactional variable (TVar) holding
-- an attribute value of 'Turtle', except the 'who_' attribute which is fixed upon turtle creation.
data Turtle = MkTurtle {
      who_ :: !Int               -- ^ fixed upon creation
    , tbreed_ :: TVar String     
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
#ifdef STATS_STM
    , ttotalstm :: IORef Int
    , tsuccstm :: IORef Int
#endif
    }

-- | The 'Patch' datatype follows a similar philosophy with the 'Turtle' (ADT).
-- Each field is a transactional variable (TVar) storing an attribute value of 'Patch', except
-- the 'pxcor_' and 'pycor_' attributes which are fixed upon patch creation.
data Patch = MkPatch {
      pxcor_ :: !Int             -- ^ fixed upon creation
    , pycor_ :: !Int             -- ^ fixed upon creation
    , pcolor_ :: TVar Double
    , plabel_ :: TVar String
    , plabel_color_ :: TVar Double
    , pvars_ :: Array Int (TVar Double)
#ifdef STATS_STM
    , ptotalstm :: IORef Int
    , psuccstm :: IORef Int
#endif
    }

-- | The 'Link' datatype follows a similar philosophy with the 'Turtle' (ADT).
-- Each field is a transactional variable (TVar) storing an attribute value of 'Link', except
-- the 'end1_', 'end2_', 'directed' attributes which are fixed upon patch creation.
data Link = MkLink {
      end1_ :: !Int              -- ^ fixed upon creation
    , end2_ :: !Int              -- ^ fixed upon creation
    , directed_ :: Bool          -- ^ fixed upon creation
    , lcolor_ :: TVar Double
    , llabel_ :: TVar String
    , llabel_color_ :: TVar Double
    , lhiddenp_ :: TVar Bool
    , lbreed_ :: TVar String
    , thickness_ :: TVar Double
    , lshape_ :: TVar String
    , tie_mode :: TVar TieMode
    , lvars_ :: Array Int (TVar Double)
#ifdef STATS_STM
    , ltotalstm :: IORef Int
    , lsuccstm :: IORef Int
#endif
    }

-- | Holds the information for the turtle, if it's pen is up or down.
--
-- Compared to NetLogo's string representation, this is an Algebraic Data Type (ADT).
-- NetLogo does not even raise a _runtime error_ for if their pen-mode string is ill-formatted,
-- and assumes that any ill-formatted string, e.g. "downn", "UP" is the same as "down"
data PenMode = Down | Up | Erase


-- | Holds the information for the link, if it ties the two turtles together, i.e.
-- one moving will result with the other moving too.
--
-- Compared to NetLogo's string representation, this is an Algebraic Data Type (ADT).
-- NetLogo does not even raise a _runtime error_ for ill-formatted tie-mode strings.
data TieMode = None | Fixed

-- | The 'Patches' ADT is an 2d-nested vector with size (max-pxcor-min-pxcor)*(max-pycor-min-pycor) 
-- that links the position (Int,Int) of a patch to its transactional attributes (patch variables) of the 'Patch' record.
--
-- The vector is pure and is initialized at the start of an HLogo program, upon calling 'run'.
type Patches = Vector Patch

-- | The 'Turtles' ADT is an 'IM.IntMap' from who indices to 'Turtle' data structures
type Turtles = IM.IntMap Turtle

-- | The 'Links' ADT is an ordered map (dictionary) from turtle Int indices (from, to) to 'Link' data structures
type Links = M.Map (Int, Int) Link

-- | Any HLogo program executes either inside 'STM' or 'IO' side-effectful monad.
-- We wrap these monads around a context mainly for two reasons:
-- 
-- 1) to have at runtime the dynamic information of the this-agent ('self') and its parent caller ('myself').
-- 2) to restrict the types of the NetLogo primitives to specifc contexts, thus turning the NetLogo dynamic language to a statically type-checked eDSL. 
type C s s' m a = ReaderT (s,s',TVar TFGen) m a

instance Eq Turtle where
    MkTurtle {who_ = w1} == MkTurtle {who_ = w2} = w1 == w2

instance Eq Link where
    -- TODO: broken for directed links because then the Link(e1,e2)=Link(e2,e1)
    MkLink {end1_ = t1e1, end2_ = t1e2} == MkLink {end1_ = t2e1, end2_ = t2e2} =  t1e1 == t2e1 && t1e2 == t2e2

instance Eq Patch where
    MkPatch {pxcor_=x1,pycor_=y1} == MkPatch {pxcor_=x2,pycor_=y2} = x1 == x2 && y1 == y2

instance Ord Turtle where
    MkTurtle {who_ = w1} `compare` MkTurtle {who_ = w2} = w1 `compare` w2

instance Ord Link where
    -- TODO: broken for undirected links because then the Link(e1,e2)>=Link(e2,e1)
    MkLink {end1_ = t1e1, end2_ = t1e2} `compare` MkLink {end1_ = t2e1, end2_ = t2e2} = (t1e1,t1e2) `compare` (t2e1,t2e2)
    
instance Ord Patch where
    -- NB: patches are compared "top-to bottom,left-ro-right" ascending
    MkPatch {pxcor_=x1,pycor_=y1} `compare` MkPatch {pxcor_=x2,pycor_=y2} = let c1 = y2 `compare` y1
                                                                            in if c1 == EQ
                                                                               then x1 `compare` x2
                                                                               else c1

instance Show Turtle where
    show (MkTurtle {who_ = tw}) = "(turtle " ++ show tw ++ ")"

instance Show Patch where
    show (MkPatch {pxcor_ = px, pycor_ = py}) = "(patch " ++ show px ++ " " ++ show py ++ ")"

instance Show Link where
    show (MkLink {end1_ = e1, end2_ = e2}) = "(link " ++ show e1 ++ " " ++ show e2 ++ ")"

instance Show Observer where
    show _ = "observer"
