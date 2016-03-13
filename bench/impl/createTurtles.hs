{-# LANGUAGE BangPatterns #-}
-- | Comparing how to create new turtles. Insert one-by-one or union the old with new
module Main where

import qualified Data.IntMap.Strict as IM
import Criterion.Main
import Criterion.Types

initialMap = IM.fromDistinctAscList [(k,()) | k <- [0..10000] ]

-- faster to union
intmapUnion m = IM.size $ m 
                `IM.union`
                IM.fromDistinctAscList [(k,()) | k <- [10001..15000] ]

intmapInserts m = IM.size $ intmapInserts' 15000 m
    where
      intmapInserts' 10000 acc = acc
      intmapInserts' !n !acc = intmapInserts' (n-1) (IM.insert n () acc)

main :: IO ()
main = defaultMain [
         bgroup "1-1" [ bench "intmapUnion"  $ nf intmapUnion initialMap
                      , bench "intmapInserts"  $ nf intmapInserts initialMap
                      ]
         ]
