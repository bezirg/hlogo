module Main where

import Test.Framework
import Math
import Patches
import Turtles
import Breeds
import Links

main = defaultMain [mathTestGroup, patchesTestGroup, turtlesTestGroup, breedsTestGroup, linksTestGroup]
        