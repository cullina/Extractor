module Main where

import Graph
import GraphAlgorithms(maxCliques, maxIndepSets, greedyIndepSet, greedyClique)
import LevGraphs(levIntEdges, levLevelIntEdges, levLevelTwoIntEdges, hWeight, levEdges, vtZeroEdges, vtZeroLevelEdges)
import LevTests(countCliqueVTH')
import Util(argMaximumsSoFar)
import System.Environment(getArgs)

main = do
  [m,g,k,n] <- fmap (map read) getArgs
  mapM_ (test m) $ gen g k n

gen :: Int -> Int -> Int -> [EdgeList Int]
gen 0 k n = map (levIntEdges k) [1..n]
gen 1 k n = map (levLevelIntEdges k) [k+1..n]
gen 2 k n = map (\x -> levLevelIntEdges x (2*x)) [1..n]
gen 3 k n = map (\x -> levLevelIntEdges x (3*x)) [1..n]
gen 4 k n = map (levLevelTwoIntEdges k) [k+1..n]
gen 5 k n = map (\x -> levLevelTwoIntEdges x (2*x)) [2..n]
gen 6 k n = map (levLevelTwoIntEdges k) [n]
gen 7 k n = map vtZeroEdges [2..n]
gen 8 k n = map (levIntEdges k) [n]
gen 9 k n = map vtZeroLevelEdges [2..n]


test :: Int -> EdgeList Int -> IO () 
test 0 = print . length . fromEdgeList
test 1 = print . maxDegree . adjListFull
test 2 = print . minDegree . adjListFull
test 3 = print . degeneracy . adjListFull
test 4 = print . maximum . map length . maxCliques . adjList
test 5 = mapM_ print . argMaximumsSoFar length . maxCliques . adjList
test 6 = print . maximum . map length . maxIndepSets . adjList
test 7 = mapM_ print . argMaximumsSoFar length . maxIndepSets . adjList
test 8 = print . length . fromFullAdj . adjListFull 
test 9 = print . length . greedyIndepSet . adjList



--test 1 k n = mapM print $ concatMap countCliqueVTH' [2..10]

--test 2 k n = g . induceSubgraphByTest ((k >=) . hWeight) . levEdges 2 $ n

--test 3 k n = mapM (f 3) [6..12]

--test 4 k n = mapM (\x -> f x (2*x)) [2..5]

