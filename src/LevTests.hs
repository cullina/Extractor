module LevTests where

import LevGraphs
import Graph(allPairs)

import Data.Set(Set, fromList, isSubsetOf, member, elems, size, intersection)
import Data.List(sort, sortBy, delete)
import Data.Function(on)
import Bit(allBitStrings)
import SubsetSelection(allSubsets, allSubsetsOf, getSubset)
import Util(keepArg2, argMaximumsSoFar)

testClique edgeSet vs = isSubsetOf cliqueEdgeSet edgeSet
  where cliqueEdgeSet = fromList . allPairs . sort $ vs
        
testCliques edgeSet = map snd . filter fst . map (keepArg2 (testClique edgeSet))

countCliques edgeSet n k = testCliques edgeSet $ allSubsetsOf k [0..n-1]

countCliqueVT n = fff n . ggg $ vtClasses n

countCliqueVTH n = zip [1..] $ fff n =<< cliqueCandidatesH n


fff n = zip [1..] . testCliques (levEdgeSet 1 n) 

ggg = map sort . sequence

cliqueCandidatesH n = map (ggg . vthClasses n) [0..(n-1) `div` 2] 

countCliqueVTH' n = fff n . ggg $ vthClasses n 1

------------------

cliqueAssistedColor :: [[Int]] -> [[Int]]
cliqueAssistedColor [] = []
cliqueAssistedColor (c:cs) = 
  cAC . sortBy (compare `on` length) $ foldr deleteVertex cs c
  where cAC [] = []
        cAC ([]:cs) = cAC cs
        cAC ([c]:cs) = cAC . sortBy (compare `on` length) $ deleteVertex c cs
        cAC cs = cs

deleteVertex :: Int -> [[Int]] -> [[Int]] 
deleteVertex v = map (delete v)

--------------------

edgeCliqueSize :: Int -> [Bool] -> [Bool]  -> (Int, Int)
edgeCliqueSize s a b =
  (size (intersection as bs), size as)
    where as = fromList $ allInsertions s a
          bs = fromList $ allInsertions s b
          

tripleCliqueSize :: Int -> [Bool] -> [Bool] -> [Bool] -> Int
tripleCliqueSize s a b c = 
  ab + ac + bc - 2 * abc
    where as = fromList $ allInsertions s a
          bs = fromList $ allInsertions s b
          cs = fromList $ allInsertions s c
          ab = size $ intersection as bs
          ac = size $ intersection as cs
          bc = size $ intersection bs cs
          abc = size $ intersection as (intersection bs cs)
          
findTCliques s n = argMaximumsSoFar tCS . allSubsetsOf 3 $ allBitStrings n
  where tCS (x:y:z:[]) = tripleCliqueSize s x y z
        tCS _ = error "Wrong subset size."
        
        
-----------------------

