module Main where

import GreedyIndSet(greedyIndSet)
import LevGraphs(levIntGraph)
import Graph(arrayGraph)
import System.Environment(getArgs)

main = getArgs >>= f

f :: [String] -> IO()
f [s,n] = print . length . greedyIndSet . arrayGraph $ levIntGraph (read s) (read n)
f _ = error "Need 2 args."
