module VT where

import Bit(bitsToInt, allBitStrings)
import SubsetSelection(allSubsets, allSubsetsOf, getSubset)
import Data.Set(Set, fromList, unions, isSubsetOf)
import Data.List
--import Data.Graph.Inductive.Graph
import Util(keepArg2)

vt = [1,1,2,2,4,6,10,16,30,52,94,172,316,586,1096,
      2048,3856,7286,13798,26216,49940,95326,182362,
      349536,671092,1290556,2485534,4793492,9256396,
      17895736,34636834,67108864,130150588,252645136,
      490853416]
     
allSingleInsertions :: [Bool] -> [[Bool]]
allSingleInsertions []     = [[True],[False]]
allSingleInsertions (b:bs) = (not b : b : bs) : map (b :) (allSingleInsertions bs)


cover :: [[Bool]] -> Set [Bool]
cover = unions . map (fromList . allSingleInsertions)

t = True
f = False

-------------------------
hWeight = length . filter id

vtWeight bs = sum (getSubset [1..] bs) `mod` (length bs + 1)

vthClass n w = let subs = allSubsets n w ++ allSubsets n (w+1)
               in \k -> filter ((k ==) . vtWeight) subs

vthClasses n w = map (vthClass n w) [0..n]

vtClass n k = filter ((k ==) . vtWeight) (allBitStrings n)

vtClasses n = map (vtClass n) [0..n]

--------------------

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

clique :: Int -> [Bool] -> [(Int,Int)]
clique s = allPairs . sort . map bitsToInt . allInsertions s

levVertices = map (keepArg2 bitsToInt) . allBitStrings

levEdges s n = fromList $ clique s =<< allBitStrings (n - s)

allPairs [] = []
allPairs (x:xs) = map ((,) x) xs ++ allPairs xs

induceSubgraph vSet = filter (bothElems vSet)
  where bothElems vSet (x, y) = elem x vSet && elem y vSet
        
testClique edgeSet vs = isSubsetOf cliqueEdgeSet edgeSet
  where cliqueEdgeSet = fromList . allPairs . sort $ vs
        
testCliques edgeSet = map snd . filter fst . map (keepArg2 (testClique edgeSet))

countCliques edgeSet n k = testCliques edgeSet $ allSubsetsOf k [0..n-1]

countCliqueVT n = fff n . ggg $ vtClasses n

countCliqueVTH n = zip [1..] $ fff n =<< cliqueCandidatesH n


fff n = zip [1..] . testCliques (levEdges 1 n) 

ggg = map sort . sequence . map (map bitsToInt)

cliqueCandidatesH n = map (ggg . vthClasses n) [0..(n-1) `div` 2] 

countCliqueVTH' n = fff n . ggg $ vthClasses n 1

