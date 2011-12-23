module Graph where

import Data.List(sort, sortBy, span, elemIndex, delete, minimumBy, maximumBy, unfoldr)
import Data.Function(on)
import Data.Set(Set, member)
import Data.Maybe(fromJust)
import Data.Ratio((%))
import Bit(showBits)
import Util(andTest, mapFst, mapSnd, mapPair)

newtype EdgeList a    = EdgeList [(a,a)] deriving Show
newtype DirEdgeList a = DirEdgeList [(a,a)] deriving Show
newtype FullAdj a     = FullAdj [(a,[a])] deriving Show
newtype FwdAdj a      = FwdAdj [(a,[a])] deriving Show

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

renameVertices :: (a -> b) -> EdgeList a -> EdgeList b
renameVertices f = EdgeList . map (mapPair f) . fromEdgeList

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

degeneracy :: Eq a => FullAdj a -> Int
degeneracy = maximum . degenSequence

degenSequence :: Eq a => FullAdj a -> [Int]
degenSequence = unfoldr dS
  where dS (FullAdj []) = Nothing
        dS g =  
          let minDegree = minimumBy (compare `on` snd) . degrees 
              (v, n) = minDegree g
          in Just (n, deleteVertex v g)

deleteVertex :: Eq a => a -> FullAdj a -> FullAdj a
deleteVertex v = FullAdj . dV v . fromFullAdj
  where 
    dV v [] = []
    dV v ((x,ys):es)
      | v == x    = dV v es
      | otherwise = (x , delete v ys) : dV v es

degrees = map (mapSnd length) . fromFullAdj

degreeData g =
  let ds = degrees g
      minim = mapFst showBits $ minimumBy (compare `on` snd) ds
      maxim = mapFst showBits $ maximumBy (compare `on` snd) ds
      n = length ds
      m = sum $ map snd ds
      av = fromIntegral m / fromIntegral n
      sparsity = fromIntegral m / fromIntegral (n * (n-1))
  in (n, minim, av, sparsity, log sparsity, maxim, degeneracy g)
     
argmaxDegree = mapFst showBits . maximumBy (compare `on` snd) . degrees

argminDegree = mapFst showBits . minimumBy (compare `on` snd) . degrees

maxDegree = maximum . map snd . degrees
minDegree = minimum . map snd . degrees