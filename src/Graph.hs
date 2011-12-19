module Graph where

import Data.List(sort, sortBy, span, elemIndex)
import Data.Function(on)
import Data.Set(Set, member)
import Data.Maybe(fromJust)
import Util(andTest, mapFst, mapSnd)

adjList :: (Ord a, Eq a) => [(a,a)] -> [(a,[a])]
adjList = removeBackLinks . adjListFull

adjListFull :: (Ord a, Eq a) => [(a,a)] -> [(a,[a])]
adjListFull = groupByFst . sort . addReverses

sortByDegree :: (Ord a, Eq a) => [(a,[a])] -> [(Int,[Int])]
sortByDegree xs =  
  let list = map fst $ sortBy (compare `on` (length . snd)) xs
  in relabelGraph (fromJust . flip elemIndex list) xs
     
adjListByDeg :: (Ord a, Eq a) => [(a,a)] -> [(Int,[Int])]
adjListByDeg = removeBackLinks . sortByDegree . adjListFull

relabelGraph :: (Ord b, Eq b) => (a -> b) -> [(a,[a])] -> [(b,[b])]
relabelGraph f = sort . map (mapFst f . mapSnd (sort . map f)) 


addReverses :: [(a,a)] -> [(a,a)]
addReverses = concatMap f
  where
    f (x,y) = [(x,y),(y,x)]

removeBackLinks :: Ord a => [(a,[a])] -> [(a,[a])]
removeBackLinks = map f
  where
    f (x, ys) = (x, filter (x <) ys)

groupByFst :: Eq a =>  [(a,a)] -> [(a,[a])]
groupByFst [] = []
groupByFst ((x,y):es) = 
  let (as, bs) = span ((x ==) . fst) es
  in (x, y : map snd as) : groupByFst bs
                      
edgeList :: [(a,[a])] -> [(a,a)]
edgeList = concatMap f
  where 
    f (x, ys) = map ((,) x) ys


induceSubgraph :: Ord a => Set a -> [(a,a)] -> [(a,a)]
induceSubgraph vSet = induceSubgraphByTest ((flip member) vSet)
        
induceSubgraphByTest :: (a -> Bool) -> [(a,a)] -> [(a,a)]
induceSubgraphByTest test = filter (andTest (test . fst) (test .snd))

------------------

matrixSquare :: Ord a => [(a,[a])] -> [(a,a)]
matrixSquare [] = []
matrixSquare (x:xs) = concatMap (f x) xs ++ matrixSquare xs
  where f (x,xs) (y,ys) = 
          if intersect xs ys
          then [(x,y)]
          else []

intersect :: Ord a => [a] -> [a] -> Bool
intersect _ [] = False
intersect [] _ = False
intersect (x:xs) (y:ys) =
  case compare x y of
    EQ -> True
    LT -> intersect xs (y:ys)
    GT -> intersect (x:xs) ys

contains :: Ord a => [a] -> [a] -> Bool
contains _ [] = True
contains [] _ = False
contains (x:xs) (y:ys) =
  case compare x y of
    EQ -> contains xs ys
    LT -> contains xs (y:ys)
    GT -> False
    
removeSubsets :: Ord a => [[a]] -> [[a]]
removeSubsets [] = []
removeSubsets (x:xs) = x : removeSubsets (filter (not . contains x) xs)

