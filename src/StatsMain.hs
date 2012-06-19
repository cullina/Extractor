module Main where

import System.Environment(getArgs)
import LevGraphs(testLevColoring)
import SubstringCounting(partialDiffStats)
import Util(mapPair, mapFst)
import Bit(showBits)

main = do
  [s,t,m,n] <- fmap (map read) getArgs
  mapM_ (\k -> print k >> f t s k) [m..n]


f :: Int -> Int -> Int -> IO ()
f t s = mapM_ (print . mapPair (mapFst showBits)) . testLevColoring (partialDiffStats t) s