module LevGraphs where

import Graph
import LevOps
import Bit(bitsToInt, allBitStrings)
import SubsetSelection(allSubsets, subsetToInteger, choose)
import Util(keepArg, keepArg2, andTest, mapFst, mapSnd, mapPair)
import Data.List(sort)
import Data.Set(fromList)
  

levVertices = map (keepArg2 bitsToInt) . allBitStrings

levEdgeSet s = fromList . concatMap (allPairs . sort) . basicCliques s

levEdges :: Int -> Int -> EdgeList [Bool]
levEdges s = organizeEdges . mergeCliques id . basicCliques s

testLevColoring :: (Ord a, Eq a) => ([Bool] -> a) -> Int -> Int -> [(([Bool], a), ([Bool], a))]
testLevColoring f s = filter (uncurry (==) . mapPair snd) . fromUnEdgeList . mergeCliques (keepArg f) . basicCliques s

levIntEdges :: Int -> Int -> EdgeList Int
levIntEdges s = organizeEdges . levIntUEdges s

levIntUEdges :: Int -> Int -> UnEdgeList Int
levIntUEdges s = mergeCliques bitsToInt . basicCliques s

levLevelEdges :: Int -> Int -> EdgeList [Bool]
levLevelEdges k = organizeEdges . mergeCliques id . levelCliques k

levLevelIntEdges :: Int -> Int -> EdgeList Int
levLevelIntEdges k n = fromCEdgeList $ levLevelIntCEdges k n

levLevelIntCEdges :: Int -> Int -> ContigEdgeList
levLevelIntCEdges k n = CEdgeList (choose n k) es 
  where 
    es = organizeEdges . mergeCliques subsetToInteger $ levelCliques k n

levLevelTwoEdges k = matrixSquare . adjListFull . levLevelEdges k

levLevelTwoIntEdges k = arraySquare . adjArray . levLevelIntCEdges k

basicCliques :: Int -> Int -> [[[Bool]]]
basicCliques s n = map (\x -> map ($ x) inFns) (allBitStrings (n - s))
  where inFns = allInsertionFns n s              

insertionStars s n = 
  map (mapFst bitsToInt . mapSnd (map bitsToInt) . keepArg (allInsertions s)) $ allBitStrings (n - s)

levelCliques :: Int -> Int -> [[[Bool]]]
levelCliques k n = map allSingleOneInsertions  (allSubsets (n - 1) (k - 1)) ++ 
                   map allSingleZeroInsertions (allSubsets (n - 1) k)

mergeCliques :: (Ord b, Eq b) => (a -> b) -> [[a]] -> UnEdgeList b
mergeCliques f = UnEdgeList . concatMap (allPairs . map f)

--------------------
vtZeroEdges :: Int -> EdgeList Int
vtZeroEdges n = renameVertices bitsToInt . induceSubgraphByTest ((0 ==) . vtWeightM (n+1)) $ levEdges 2 n

levLevelTwoEdges2 k = renameVertices bitsToInt . induceSubgraphByTest ((k ==) . hWeight) . levEdges 2 

interval :: Int -> Int -> Int -> Bool
interval low high = andTest (low <=) (high >=)

firstLevels :: Int -> Int -> EdgeList Int
firstLevels s = renameVertices bitsToInt . induceSubgraphByTest (interval 1 (s+1) . hWeight) . levEdges s 

midLevels :: Int -> EdgeList Int
midLevels n = 
  let q = div n 2
  in renameVertices bitsToInt . induceSubgraphByTest (interval (q - 1) (q + 1) . hWeight) $ levEdges 2 n
           

vtZeroLevelEdges :: Int -> EdgeList Int
vtZeroLevelEdges n = renameVertices bitsToInt . induceSubgraphByTest ((0 ==) . vtWeightM (n+1)) $ levLevelTwoEdges n (2*n)

-------------------
