module Main where

import System.Environment(getArgs)
import Color
import QaryGraphs
import Graph(adjList)


main = do
  [q,n,k] <- fmap (map read) getArgs
  mapM_ print . map colorClasses . color k . adjList $ qIntEdges q n

