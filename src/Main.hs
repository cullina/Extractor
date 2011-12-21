module Main where

import Graph
import GraphAlgorithms(maxCliques)
import LevGraphs(levIntEdges, levLevelIntEdges, levLevelTwoEdges, hWeight, levEdges)
import LevTests(countCliqueVTH')
import Util(argMaximumsSoFar)
import System.Environment(getArgs)

main = do
  [m,g,k,n] <- fmap (map read) getArgs
  test m $ gen g k n

gen 0 k n = map (levIntEdges k) [1..n]
gen 1 k n = map (levLevelIntEdges k) [k+1..n]
gen 2 k n = map (\x -> levLevelIntEdges x (2*x)) [1..n]
gen 3 k n = map (\x -> levLevelIntEdges x (3*x)) [1..n]
gen 4 k n = map (levLevelTwoEdges k) [k+1..n]

test 0 = mapM (print . length . fromEdgeList)
test 1 = mapM (print . maxDegree . adjListFull)
test 2 = mapM (print . minDegree . adjListFull)
test 3 = mapM (print . degeneracy . adjListFull)
test 4 = mapM (print . maximum . map length . maxCliques . adjListByDeg)
test 5 = mapM print . concatMap (argMaximumsSoFar length . maxCliques . adjListByDeg)
test 6 = mapM print . concatMap (argMaximumsSoFar length . maxCliques . adjListFull)





--test 1 k n = mapM print $ concatMap countCliqueVTH' [2..10]

--test 2 k n = g . induceSubgraphByTest ((k >=) . hWeight) . levEdges 2 $ n

--test 3 k n = mapM (f 3) [6..12]

--test 4 k n = mapM (\x -> f x (2*x)) [2..5]

