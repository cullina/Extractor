module Main where

import Graph
import GraphAlgorithms(allCliques, maxCliqueSize)
import LevGraphs(levIntEdges,levLevelTwoEdges)
import LevTests(countCliqueVTH')

main = testThree

testOne = mapM print $ concatMap countCliqueVTH' [2..10]

testTwo = mapM print . allCliques . adjList $ levIntEdges 2 5

testThree = f 9
  where f = mapM (print . length) . allCliques . adjListByDeg . levLevelTwoEdges 3