module Main where

import System.Environment(getArgs)
import QaryGraphs
import Graph(adjListFull)
import GraphAlgorithms(allColoringsL)
import Util(incrementalLength)

main = do
  [q,n,k] <- fmap (map read) getArgs
  incrementalLength . allColoringsL k . adjListFull $ qIntEdges q n
