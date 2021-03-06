module Main where

import Graph
import QaryGraphs(qLevelIntEdges)
import GraphAlgorithms(allBigIndepSets)
import System.Environment(getArgs)

main = getArgs >>= f

f :: [String] -> IO()
f [a,b,c,l] = test (read l) . adjListFull . qLevelIntEdges $ [read a, read b, read c]
f _ = error "Need 3 args."
  

test l = mapM_ print . allBigIndepSets l . removeBackLinks

