module LevGraphs where

import Graph

import Bit(bitsToInt, allBitStrings, xor)
import SubsetSelection(allSubsets, allSubsetsOf, getSubset)
import Util(keepArg2, fastNub)
import Data.List(sort, mapAccumL)
import Data.Set(fromList)
import Control.Monad((<=<))

allSingleInsertions :: [Bool] -> [[Bool]]
allSingleInsertions []     = [[True],[False]]
allSingleInsertions (b:bs) = (not b : b : bs) : map (b :) (allSingleInsertions bs)

allSingleZeroInsertions :: [Bool] -> [[Bool]]
allSingleZeroInsertions []         = [[False]]
allSingleZeroInsertions (True:bs)  = (False : True : bs) : map (True :) (allSingleZeroInsertions bs)
allSingleZeroInsertions (False:bs) = map (False :) (allSingleZeroInsertions bs)

allSingleOneInsertions :: [Bool] -> [[Bool]]
allSingleOneInsertions []         = [[True]]
allSingleOneInsertions (True:bs)  = map (True :) (allSingleOneInsertions bs)
allSingleOneInsertions (False:bs) = (True : False : bs) : map (False :) (allSingleOneInsertions bs)


atMostSOnes n s = allSubsets n =<< [0..s]

insertion ps [] = ps
insertion (True:ps)  (b:bs) = not b : insertion ps (b:bs)
insertion (False:ps) (b:bs) = b : insertion ps bs
insertion [] _ = []

insertion2 :: [Bool] -> [Bool] -> [Bool]
insertion2 bs = snd . mapAccumL f bs
  where f []       p     = ([], p)
        f bs@(b:_) True  = (bs, not b)
        f (b:bs)   False = (bs, b)


allInsertions :: Int -> [Bool] -> [[Bool]]
allInsertions s bs = map (insertion2 bs) (atMostSOnes (length bs + s) s)

allDeletions :: Ord a => Int -> [a] ->[[a]]
allDeletions s bs = fastNub . map (getSubset bs . map not) . allSubsets (length bs) $ s

levVertices = map (keepArg2 bitsToInt) . allBitStrings

levEdgeSet s = fromList . concatMap (allPairs . sort) . basicCliques s

levEdges s = mergeCliques id . basicCliques s

levIntEdges s = mergeCliques bitsToInt . basicCliques s

levLevelEdges k = mergeCliques id . levelCliques k

levLevelIntEdges k = mergeCliques bitsToInt . levelCliques k

levLevelTwoEdges k = matrixSquare . adjListFull . levLevelIntEdges k

basicCliques s n = map (allInsertions s) (allBitStrings (n - s))

levelCliques :: Int -> Int -> [[[Bool]]]
levelCliques k n = map allSingleOneInsertions  (allSubsets (n - 1) (k - 1)) ++ 
                   map allSingleZeroInsertions (allSubsets (n - 1) k)

mergeCliques :: (Ord b, Eq b) => (a -> b) -> [[a]] -> EdgeList b
mergeCliques f = EdgeList . fastNub . concatMap (allPairs . sort . map f)

-------------------------
hWeight = length . filter id

vtSum = sum . getSubset [1..]

vtWeight bs = vtWeightM (length bs + 1) bs

vtWeightM m bs = vtSum bs `mod` m

vthClass n w = let subs = allSubsets n w ++ allSubsets n (w+1)
               in \k -> filter ((k ==) . vtWeight) subs

vthClasses n w = map (vthClass n w) [0..n]

vtClass n k = filter ((k ==) . vtWeight) (allBitStrings n)

vtClasses n = map (vtClass n) [0..n]

vtLevelClass n k a = filter ((a ==) . vtWeightM (1 + (max k (n - k)))) (allSubsets n k) 

genWeight ws max = (`mod` max) . sum . getSubset ws 

--------------------
vtZeroEdges :: Int -> EdgeList Int
vtZeroEdges n = renameVertices bitsToInt . induceSubgraphByTest ((0 ==) . vtWeightM (n+1)) . levEdges 2 $ n

leLevelTwoEdges2 k = renameVertices bitsToInt . induceSubgraphByTest ((k ==) . hWeight) . levEdges 2 

-------------------

neighborhood :: Int -> [Bool] -> Int
neighborhood s = length . fastNub . map bitsToInt . (allInsertions s <=< allDeletions s)

----

fromBlocks :: [Int] -> [Bool]
fromBlocks ns = zipWith xor alt . concatMap (uncurry replicate) $ zip ns alt
  where alt = concat (repeat [False,True])
        
evenBlocks k m = fromBlocks $ replicate k m