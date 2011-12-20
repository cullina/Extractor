module Graph where

import Data.List(sort, sortBy, span, elemIndex)
import Data.Function(on)
import Data.Set(Set, member)
import Data.Maybe(fromJust)
import Util(andTest, mapFst, mapSnd)

newtype EdgeList a    = EdgeList [(a,a)]
newtype DirEdgeList a = DirEdgeList [(a,a)]
newtype FullAdj a     = FullAdj [(a,[a])]
newtype FwdAdj a      = FwdAdj [(a,[a])]

fromEdgeList (EdgeList x)       = x
fromDirEdgeList (DirEdgeList x) = x
fromFullAdj (FullAdj x)         = x
fromFwdAdj (FwdAdj x)           = x

fstVertex :: (FwdAdj a) -> Maybe ((a,[a]), FwdAdj a) 
fstVertex (FwdAdj []) = Nothing
fstVertex (FwdAdj (v:vs)) = Just (v, FwdAdj vs)

adjList :: (Ord a, Eq a) => EdgeList a -> FwdAdj a
adjList = removeBackLinks . adjListFull

adjListFull :: (Ord a, Eq a) => EdgeList a -> FullAdj a
adjListFull = FullAdj . groupByFst . sort . fromDirEdgeList . addReverses

sortEdges :: (Ord a) => DirEdgeList a -> DirEdgeList a
sortEdges (DirEdgeList es) = DirEdgeList (sort es)

sortByDegree :: (Ord a, Eq a) => FullAdj a -> FullAdj Int
sortByDegree (FullAdj xs) =  
  let list = map fst $ sortBy (flip (compare `on` (length . snd))) xs
  in FullAdj $ relabelGraph (fromJust . flip elemIndex list) xs
     
adjListByDeg :: (Ord a, Eq a) => EdgeList a -> FwdAdj Int
adjListByDeg = removeBackLinks . sortByDegree . adjListFull

relabelGraph :: (Ord b, Eq b) => (a -> b) -> [(a,[a])] -> [(b,[b])]
relabelGraph f = sort . map (mapFst f . mapSnd (sort . map f)) 


addReverses :: EdgeList a -> DirEdgeList a
addReverses (EdgeList es) = DirEdgeList $ concatMap f es
  where
    f (x,y) = [(x,y),(y,x)]

removeBackLinks :: Ord a => FullAdj a -> FwdAdj a
removeBackLinks = FwdAdj . map f . fromFullAdj
  where
    f (x, ys) = (x, filter (x <) ys)

groupByFst :: Eq a => [(a,a)] -> [(a,[a])]
groupByFst [] = []
groupByFst ((x,y):es) = 
  let (as, bs) = span ((x ==) . fst) es
  in (x, y : map snd as) : groupByFst bs
                      
edgeList :: FwdAdj a -> EdgeList a
edgeList (FwdAdj xs) = EdgeList $ concatMap f xs
  where 
    f (x, ys) = map ((,) x) ys


induceSubgraph :: Ord a => Set a -> EdgeList a -> EdgeList a
induceSubgraph vSet = induceSubgraphByTest ((flip member) vSet)
        
induceSubgraphByTest :: (a -> Bool) -> EdgeList a -> EdgeList a
induceSubgraphByTest test (EdgeList es) = 
  EdgeList $ filter (andTest (test . fst) (test .snd)) es

------------------

matrixSquare :: Ord a => FullAdj a -> EdgeList a
matrixSquare (FullAdj xs) = EdgeList $ mS xs
  where 
    mS [] = []
    mS (x:xs) = concatMap (f x) xs ++ mS xs
    f (x,xs) (y,ys) = 
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

