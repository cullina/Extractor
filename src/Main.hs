module Main where

import Graph
import GraphAlgorithms(maxCliques)
import LevGraphs(levIntEdges, levLevelTwoEdges, hWeight, levEdges)
import LevTests(countCliqueVTH')
import Util(argMaximumsSoFar)
import System.Environment(getArgs)

main = do
  [k,n] <- fmap (map read) getArgs
  testSix k n

testOne = mapM print $ concatMap countCliqueVTH' [2..10]

testTwo k = g . induceSubgraphByTest ((k >=) . hWeight) . levEdges 2

testThree = mapM (f 3) [6..12]

testFour = mapM (\x -> f x (2*x)) [2..5]

testFive = f 4 10

f k = g . levLevelTwoEdges k

g :: (Ord a) => EdgeList a -> IO [()]
g = mapM print . argMaximumsSoFar length . maxCliques . adjListByDeg

testSix k n = mapM (print . degreeData . adjListFull . levEdges k) [1..n]