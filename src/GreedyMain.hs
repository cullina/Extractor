module Main where

import GreedyIndSet(greedyIndSet)
import Graph(arrayGraph, fromUnEdgeList, allPairs, UnEdgeList(..))
import LevGraphs(levIntUEdges)
import System.Environment(getArgs)

main = getArgs >>= f

f :: [String] -> IO()
f [s,n] = print . length . greedyIndSet . arrayGraph $ levIntUEdges (read s) (read n)
f _ = error "Need 2 args."

h :: [String] -> IO()
h [n] = print . length . greedyIndSet . arrayGraph . UnEdgeList $ allPairs [0..(read n)]
h _ = error "Need 1 arg."

g [s,n] = print . foldr p (0,0) . fromUnEdgeList $ levIntUEdges (read s) (read n)
g _ = error "Need 2 args."

p (w,x) (y,z) = (w+y,x+z)