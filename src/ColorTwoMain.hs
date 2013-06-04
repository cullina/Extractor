module Main where

import System.Environment(getArgs)
import QaryGraphs
import Graph(adjList)
import GraphAlgorithms(allColorings)
import Util(incrementalLength)

main = do
  [q,n,k] <- fmap (map read) getArgs
  incrementalLength . allColorings k . adjList $ qIntEdges q n
