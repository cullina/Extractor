module Main where

import Graph
import GraphAlgorithms(allCliques, maxCliqueSize, maxCliques)
import LevGraphs(levIntEdges,levLevelTwoEdges)
import LevTests(countCliqueVTH')
import Util(argMaximumsSoFar)
import System.Environment(getArgs)

main = do
  [k,n] <- fmap (map read) getArgs
  f k n

testOne = mapM print $ concatMap countCliqueVTH' [2..10]

--testTwo = mapM print . allCliques . adjList $ levIntEdges 2 5

testThree = mapM (f 3) [6..12]

testFour = mapM (\x -> f x (2*x)) [2..5]

testFive = f 4 10

f k = mapM print . argMaximumsSoFar length . maxCliques . adjListByDeg . levLevelTwoEdges k