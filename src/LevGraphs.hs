module LevGraphs where

import Graph
import LevOps
import Bit(bitsToInt, allBitStrings)
import SubsetSelection(allSubsets, subsetToInteger, choose)
import Util(keepArg, keepArg2, andTest, mapFst, mapSnd, mapPair)
import Data.List(sort)

levVertices = map (keepArg2 bitsToInt) . allBitStrings

levGraph :: Int -> Int -> FullAdj [Bool]
levGraph s = fromCliques . basicCliques s

levIntGraph :: Int -> Int -> FullAdj Int
levIntGraph s = fromCliques . fmap bitsToInt . basicCliques s

levEdges :: Int -> Int -> EdgeList [Bool]
levEdges s = organizeEdges . mergeCliques . basicCliques s

levIntEdges :: Int -> Int -> EdgeList Int
levIntEdges s = organizeEdges . levIntUEdges s

levIntUEdges :: Int -> Int -> UnEdgeList Int
levIntUEdges s = mergeCliques . fmap bitsToInt . basicCliques s

levLevelGraph :: Int -> Int -> FullAdj [Bool]
levLevelGraph k = fromCliques . levelCliques k

levLevelEdges :: Int -> Int -> EdgeList [Bool]
levLevelEdges k = organizeEdges . mergeCliques . levelCliques k

levLevelIntEdges :: Int -> Int -> EdgeList Int
levLevelIntEdges k n = fromCEdgeList $ levLevelIntCEdges k n

levLevelIntCEdges :: Int -> Int -> ContigEdgeList
levLevelIntCEdges k n = CEdgeList (choose n k) es 
  where 
    es = organizeEdges . mergeCliques . fmap subsetToInteger $ levelCliques k n

levLevelTwoEdges k = matrixSquare . adjListFull . levLevelEdges k

levLevelTwoIntEdges k = arraySquare . adjArray . levLevelIntCEdges k

basicCliques :: Int -> Int -> CliqueList [Bool]
basicCliques s n = CliqueList $ map (\x -> map ($ x) inFns) (allBitStrings (n - s))
  where inFns = allInsertionFns n s              

insertionStars s n = 
  map (mapFst bitsToInt . mapSnd (map bitsToInt) . keepArg (allInsertions s)) $ allBitStrings (n - s)

levelCliques :: Int -> Int -> CliqueList [Bool]
levelCliques k n = CliqueList $ map allSingleOneInsertions  (allSubsets (n - 1) (k - 1)) ++ 
                   map allSingleZeroInsertions (allSubsets (n - 1) k)

mergeCliques :: (Ord a, Eq a) => CliqueList a -> UnEdgeList a
mergeCliques = UnEdgeList . concatMap (allPairs . sort) . fromCliqueList

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

testLevColoring :: (Ord a, Eq a) => ([Bool] -> a) -> Int -> Int -> [(([Bool], a), ([Bool], a))]
testLevColoring f s = filter (uncurry (==) . mapPair snd) . fromUnEdgeList . mergeCliques . fmap (keepArg f) . basicCliques s