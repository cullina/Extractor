module Main where

import Graph
import GraphDegree
import GraphAlgorithms(maxCliques, maxIndepSets, greedyIndepSet, allMaxIndepSets)
import LevGraphs(levIntGraph, levLevelIntEdges, levLevelTwoIntEdges, vtZeroEdges, vtZeroLevelEdges, firstLevels, midLevels)
import QaryGraphs(qMiddleLevelIntEdges)
import Util(argMaximumsSoFar)
import System.Environment(getArgs)

main = getArgs >>= f

f :: [String] -> IO()
f [t,g,k,n] = mapM_ (test t) $ gen (read g) (read k) (read n)
f _ = error "Need 4 args."

gen :: Int -> Int -> Int -> [FullAdj Int]
gen 0 s n = map (levIntGraph s) [1..n]
gen 1 s n = map (levIntGraph s) [n]
gen 2 k n = map (adjListFull . levLevelIntEdges k) [k+1..n]
gen 3 _ n = map (\x -> adjListFull $ levLevelIntEdges x (2*x)) [1..n]
gen 4 _ n = map (\x -> adjListFull $ levLevelIntEdges x (3*x)) [1..n]
gen 5 k n = map (levLevelTwoIntEdges k) [k+1..n]
gen 6 _ n = map (\x -> levLevelTwoIntEdges x (2*x)) [2..n]
gen 7 k n = map (levLevelTwoIntEdges k) [n]
gen 8 _ n = map (adjListFull . vtZeroEdges) [2..n]
gen 9 _ n = map (adjListFull . vtZeroLevelEdges) [2..n]
gen 10 k n = map (adjListFull . firstLevels k) [(k+2)..n]
gen 11 _ n = map (adjListFull . midLevels) [n]
gen 12 q k = map adjListFull [qMiddleLevelIntEdges q k]
gen _ _ _ = undefined


test :: String -> FullAdj Int -> IO () 
test "totalDegree" = print . totalDegree
test "maxDegree"   = print . maxDegree
test "minDegree"   = print . minDegree
test "degeneracy"  = print . degeneracy
test "cliques"     = print . maximum . map length . maxCliques . removeBackLinks
test "cliques2"    = mapM_ print . argMaximumsSoFar length . maxCliques . removeBackLinks
test "indep"       = print . maximum . map length . maxIndepSets . removeBackLinks
test "allIndep"    = print . maximum . map length . allMaxIndepSets . removeBackLinks
test "indep2"      = mapM_ print . argMaximumsSoFar length . maxIndepSets . removeBackLinks
test "allIndep2"      = mapM_ print . argMaximumsSoFar length . allMaxIndepSets . removeBackLinks
test "vertices"    = print . length . fromFullAdj 
test "indepGreedy" = print . length . greedyIndepSet . removeBackLinks


test _ = undefined


--test 1 k n = mapM print $ concatMap countCliqueVTH' [2..10]

--test 2 k n = g . induceSubgraphByTest ((k >=) . hWeight) . levEdges 2 $ n

--test 3 k n = mapM (f 3) [6..12]

--test 4 k n = mapM (\x -> f x (2*x)) [2..5]

