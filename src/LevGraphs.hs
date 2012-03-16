module LevGraphs where

import Graph
import LevOps
import Bit(bitsToInt, allBitStrings)
import SubsetSelection(allSubsets, subsetToInteger, choose)
import Util(keepArg, keepArg2, andTest, mapFst, mapSnd)
import ListSet(listSetFromList)
import Data.List(sort)
import Data.Set(fromList)
  

levVertices = map (keepArg2 bitsToInt) . allBitStrings

levEdgeSet s = fromList . concatMap (allPairs . sort) . basicCliques s

levEdges s = mergeCliques id . basicCliques s

levIntEdges s = mergeCliques bitsToInt . basicCliques s

levLevelEdges k = mergeCliques id . levelCliques k

levLevelIntEdges :: Int -> Int -> EdgeList Int
levLevelIntEdges k n = fromCEdgeList $ levLevelIntCEdges k n

levLevelIntCEdges :: Int -> Int -> ContigEdgeList
levLevelIntCEdges k n = CEdgeList (choose n k) es 
  where 
    es = mergeCliques subsetToInteger $ levelCliques k n

levLevelTwoEdges k = matrixSquare . adjListFull . levLevelEdges k

levLevelTwoIntEdges k = arraySquare . adjArray . levLevelIntCEdges k

basicCliques s n = map (allInsertions s) (allBitStrings (n - s))

insertionStars s n = 
  map (mapFst bitsToInt . mapSnd (map bitsToInt) . keepArg (allInsertions s)) $ allBitStrings (n - s)

levelCliques :: Int -> Int -> [[[Bool]]]
levelCliques k n = map allSingleOneInsertions  (allSubsets (n - 1) (k - 1)) ++ 
                   map allSingleZeroInsertions (allSubsets (n - 1) k)

mergeCliques :: (Ord b, Eq b) => (a -> b) -> [[a]] -> EdgeList b
mergeCliques f = EdgeList . listSetFromList . concatMap (allPairs . sort . map f)

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
