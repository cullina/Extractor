module Main where

import System.Environment(getArgs)
import QaryGraphs
import Graph(adjList)
import GraphAlgorithms(allColorings)

main = do
  [q,n,k] <- fmap (map read) getArgs
  mapM (print . (\_ -> ())) . allColorings k . adjList $ qIntEdges q n
