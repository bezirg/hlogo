{-# LANGUAGE DeriveDataTypeable #-}

module Framework.Logo.Conf where

import System.IO.Unsafe
import System.Console.CmdArgs

-- easier to have a top-level state for the conf
conf = unsafePerformIO getConf

getConf = cmdArgs confOpt

data Conf = Conf {
      max_pxcor_ :: Int
    , max_pycor_ :: Int
    , min_pxcor_ :: Int
    , min_pycor_ :: Int
    , patch_size_ :: Int
    , origin_location_ :: String
    , horizontal_wrap_ :: Bool
    , vertical_wrap_ :: Bool
    , split_ :: String
  } deriving (Show, Eq, Data, Typeable)

confOpt = Conf {
            max_pxcor_ = 16 &= name "max-pxcor"  &= typ "NUM" &= help "Setting the max_pxcor"
          , max_pycor_ = 16 &= name "max-pycor"  &= typ "NUM" &= help "Setting the max_pycor"
          , min_pxcor_ = (-16) &= name "mix-pxcor"  &= typ "NUM" &= help "Setting the max_pxcor"
          , min_pycor_ = (-16) &= name "mix-pycor"  &= typ "NUM" &= help "Setting the max_pycor"
          , patch_size_ = 13 &= name "patch-size" &= typ "NUM" &= help "Set the size of the patch in pixels"
          , origin_location_ = "center" &= explicit &= name "o" &= name "origin-location" &= help "Setting the origin location"
          , horizontal_wrap_ = def &= explicit &= name "h" &= name "horizontal-wrap" &= help "When set, enables horizontal wrapping"
          , vertical_wrap_ = def &= explicit &= name "v" &= name "vertical-wrap" &= help "When set, enables vertical wrapping"
          , split_ = "none" &= explicit &= name "split" &= help "Set the spatial clustering of agents to threads. Options: none, horizontal, vertical, both"
          } &= program "hlogo" &= help "HLogo framework" &= summary "HLogo v0.0.1, (C) Nikolaos Bezirgiannis, Ilias Sakellariou"


