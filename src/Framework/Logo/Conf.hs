{-# LANGUAGE DeriveDataTypeable #-}

module Framework.Logo.Conf where

import System.IO.Unsafe
import System.Console.CmdArgs

-- easier to have a top-level state for the conf
conf = let c = unsafePerformIO getConf
       in case c of
            -- trick to check for unset coordinates
            Conf {max_pxcor_ = 0, max_pycor_ = 0 , min_pxcor_ =0, min_pycor_ = 0, patch_size_ = 0} -> c {max_pxcor_ = 16, max_pycor_ = 16, min_pxcor_ = -16, min_pycor_ = -16, patch_size_ = 13}
            _ -> c

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
  } deriving (Show, Eq, Data, Typeable)

confOpt = Conf {
            max_pxcor_ = def &= name "max-pxcor"  &= typ "NUM" &= opt (16 :: Int) &= help "Setting the max_pxcor"
          , max_pycor_ = def &= name "max-pycor"  &= typ "NUM" &= opt (16 :: Int) &= help "Setting the max_pycor"
          , min_pxcor_ = def &= name "mix-pxcor"  &= typ "NUM" &= opt (-16 :: Int) &= help "Setting the max_pxcor"
          , min_pycor_ = def &= name "mix-pycor"  &= typ "NUM" &= opt (-16 :: Int) &= help "Setting the max_pycor"
          , patch_size_ = def &= name "patch-size" &= typ "NUM" &= opt (13 :: Int) &= help "Set the size of the patch in pixels"
          , origin_location_ = def &= explicit &= name "o" &= name "origin-location" &= opt "center" &= help "Setting the origin location"
          , horizontal_wrap_ = def &= explicit &= name "h" &= name "horizontal-wrap" &= help "When set, enables horizontal wrapping"
          , vertical_wrap_ = def &= explicit &= name "v" &= name "vertical-wrap" &= help "When set, enables vertical wrapping"
          } &= program "hlogo" &= help "HLogo framework" &= summary "HLogo v0.0.1, (C) Nikolaos Bezirgiannis, Ilias Sakellariou"


