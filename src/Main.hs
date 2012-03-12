module Main where

import Graph
import GraphDegree
import GraphAlgorithms(maxCliques, maxIndepSets, greedyIndepSet)
import LevGraphs(levIntEdges, levLevelIntEdges, levLevelTwoIntEdges, vtZeroEdges, vtZeroLevelEdges, firstLevels, midLevels)
import Util(argMaximumsSoFar)
import System.Environment(getArgs)

main = do
  [m,g,k,n] <- fmap (map read) getArgs
  mapM_ (test m) $ gen g k n

gen :: Int -> Int -> Int -> [FullAdj Int]
gen 0 k n = map (adjListFull . levIntEdges k) [1..n]
gen 1 k n = map (adjListFull . levLevelIntEdges k) [k+1..n]
gen 2 _ n = map (\x -> adjListFull $ levLevelIntEdges x (2*x)) [1..n]
gen 3 _ n = map (\x -> adjListFull $ levLevelIntEdges x (3*x)) [1..n]
gen 4 k n = map (levLevelTwoIntEdges k) [k+1..n]
gen 5 _ n = map (\x -> levLevelTwoIntEdges x (2*x)) [2..n]
gen 6 k n = map (levLevelTwoIntEdges k) [n]
gen 7 _ n = map (adjListFull . vtZeroEdges) [2..n]
gen 8 k n = map (adjListFull . levIntEdges k) [n]
gen 9 _ n = map (adjListFull . vtZeroLevelEdges) [2..n]
gen 10 k n = map (adjListFull . firstLevels k) [(k+2)..n]
gen 11 _ n = map (adjListFull . midLevels) [n]
gen _ _ _ = undefined


test :: Int -> FullAdj Int -> IO () 
test 0 = print . totalDegree
test 1 = print . maxDegree
test 2 = print . minDegree
test 3 = print . degeneracy
test 4 = print . maximum . map length . maxCliques . removeBackLinks
test 5 = mapM_ print . argMaximumsSoFar length . maxCliques . removeBackLinks
test 6 = print . maximum . map length . maxIndepSets . removeBackLinks
test 7 = mapM_ print . argMaximumsSoFar length . maxIndepSets . removeBackLinks
test 8 = print . length . fromFullAdj 
test 9 = print . length . greedyIndepSet . removeBackLinks


test _ = undefined


--test 1 k n = mapM print $ concatMap countCliqueVTH' [2..10]

--test 2 k n = g . induceSubgraphByTest ((k >=) . hWeight) . levEdges 2 $ n

--test 3 k n = mapM (f 3) [6..12]

--test 4 k n = mapM (\x -> f x (2*x)) [2..5]

