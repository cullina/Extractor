module Main where

import GreedyIndSet(greedyIndSet)
import LevGraphs(levIntArrayGraph)
import System.Environment(getArgs)

main = getArgs >>= f

f :: [String] -> IO()
f [s,n] = print . length . greedyIndSet $ levIntArrayGraph (read s) (read n)
f _ = error "Need 2 args."
