{-# LANGUAGE DeriveDataTypeable, TemplateHaskell #-}
{-# OPTIONS_HADDOCK show-extensions #-}
-- | 
-- Module      :  Language.Logo.Conf
-- Copyright   :  (c) 2013-2016, the HLogo team
-- License     :  BSD3
-- Maintainer  :  Nikolaos Bezirgiannis <bezirgia@cwi.nl>
-- Stability   :  experimental
--
-- The getopt-like command-line (HLogo-runtime) args passed to every HLogo program
module Language.Logo.CmdOpt
    (
      CmdOpt (..)
    , cmdOpt
    ) where

import System.Console.CmdArgs
import System.IO.Unsafe (unsafePerformIO)

{-# NOINLINE cmdOpt #-}
cmdOpt :: CmdOpt
cmdOpt = unsafePerformIO (cmdArgs cmdOptSpec)

-- | All the possible command-line (HLogo-runtime) args and their types
data CmdOpt = CmdOpt {
      max_pxcor_ :: Int
    , max_pycor_ :: Int
    , min_pxcor_ :: Int
    , min_pycor_ :: Int
    , origin_location_ :: Origin
    , horizontal_wrap_ :: Bool
    , vertical_wrap_ :: Bool
    , split_ :: Split
    , patch_size_ :: Int
  } deriving (Show, Data, Typeable)

-- | Where lies the origin location (0,0) in the 2d-space
data Origin = Center
            | BottomEdge
            | TopEdge
            | RightEdge
            | LeftEdge
            | BottomLeftCorner
            | BottomRightCorner
            | TopLeftCorner
            | TopRightCorner
            | Custom
            deriving (Show, Eq, Data, Typeable, Enum)

-- | This declares the strategy of spatially slicing an agentset (that is passed to 'ask'/'of_'/'with')
-- and giving a piece of the agentset to a different thread.
--
-- Since its model will work better with a different splitting strategy, it is left
-- to the user to figure out the best-suited that will yield the minimum number of 
-- transaction aborts/retries. You can recompile the runtime with the flag "stats-stm"
-- to debug the number of retries during an execution.
data Split = HorizontalSplit
           | VerticalSplit
           | ChunkSplit
           | RoundRobinSplit
           deriving (Show, Data, Typeable, Enum)

-- | Internal
--
-- The cmdargs command-line specification.
cmdOptSpec :: CmdOpt
cmdOptSpec = CmdOpt {
            max_pxcor_ = 16 
                         &= explicit 
                         &= name "max-pxcor"  
                         &= typ "NUM" 
                         &= help "Setting the max_pxcor"
          , max_pycor_ = 16 
                         &= explicit 
                         &= name "max-pycor"  
                         &= typ "NUM" 
                         &= help "Setting the max_pycor"
          , min_pxcor_ = (-16) 
                         &= explicit 
                         &= name "min-pxcor"  
                         &= typ "NUM" 
                         &= help "Setting the max_pxcor"
          , min_pycor_ = (-16) 
                         &= explicit 
                         &= name "min-pycor"  
                         &= typ "NUM" 
                         &= help "Setting the max_pycor"
          , origin_location_ = Center 
                               &= explicit 
                               &= name "origin-location" 
                               &= help "Setting the origin location"
          , horizontal_wrap_ = False 
                               &= explicit 
                               &= name "horizontal-wrap" 
                               &= help "When set, enables world horizontal wrapping"
          , vertical_wrap_ = False 
                             &= explicit 
                             &= name "vertical-wrap" 
                             &= help "When set, enables world vertical wrapping"
          , split_ = ChunkSplit 
                     &= explicit 
                     &= name "split" 
                     &= help "Set the spatial clustering of agents to threads. Options: none, horizontal, vertical, both"
          , patch_size_ = 13 
                          &= explicit 
                          &= name "patch-size" 
                          &= typ "NUM" 
                          &= help "Set the size of the patch in pixels (Visualization-related-only, does not affect the model's execution)"
          } 
          &= program "hlogo" 
          &= help "HLogo framework" 
          &= helpArg [explicit, name "h", name "help"]
          &= summary ("model compiled with HLogo v0.1.9, (C) Nikolaos Bezirgiannis, Wishnu Prasetya, Ilias Sakellariou") -- summary is --version


