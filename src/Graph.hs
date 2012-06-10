module Graph where

import Data.List(sort, sortBy)
import Data.Array(Array, listArray, (!), range, bounds)
import Data.Function(on)
import Data.Set(Set, member)
import Util(andTest, mapFst, mapSnd, mapPair, toStandardInt)
import ListSet


newtype EdgeList a    = EdgeList [(a,a)] deriving Show
newtype DirEdgeList a = DirEdgeList [(a,a)] deriving Show
newtype FullAdj a     = FullAdj [(a,[a])] deriving Show
newtype FwdAdj a      = FwdAdj [(a,[a])] deriving Show
data ContigEdgeList = CEdgeList Int (EdgeList Int)


fromEdgeList    (EdgeList x)    = x
fromDirEdgeList (DirEdgeList x) = x
fromFullAdj     (FullAdj x)     = x
fromFwdAdj      (FwdAdj x)      = x
fromCEdgeList   (CEdgeList _ x) = x


fstVertex :: FwdAdj a -> Maybe ((a,[a]), FwdAdj a) 
fstVertex (FwdAdj [])     = Nothing
fstVertex (FwdAdj (v:vs)) = Just (v, FwdAdj vs)

adjListFull :: (Ord a, Eq a) => EdgeList a -> FullAdj a
adjListFull es = FullAdj . map (neighbors es) $ vertexList es

neighbors :: Eq a => EdgeList a -> a -> (a,[a])
neighbors g x = (x, concatMap (f x) $ fromEdgeList g)
  where 
    f x (y,z)
      | x == y    = [z]
      | x == z    = [y]
      | otherwise = []

adjArray :: ContigEdgeList -> Array Int [Int]
adjArray (CEdgeList n es) = listArray (0,n-1) . map (snd . neighbors es) $ [0..n-1]

deleteVertexEL :: Eq a => a -> EdgeList a -> EdgeList a
deleteVertexEL x = EdgeList . filter (f x) . fromEdgeList
  where
    f x (y,z) = (x /= y) && (x /= z)

adjList :: (Ord a, Eq a) => EdgeList a -> FwdAdj a
adjList = removeBackLinks . adjListFull

adjListFull2 :: (Ord a, Eq a) => EdgeList a -> FullAdj a
adjListFull2 = FullAdj . groupByFst . sort . fromDirEdgeList . addReverses

sortByDegree :: (Ord a, Eq a) => FullAdj a -> FullAdj Int
sortByDegree (FullAdj xs) =  
  let list = map fst $ sortBy (flip (compare `on` (length . snd))) xs
  in FullAdj $ relabelGraph (toStandardInt list) xs
     
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

vertexList :: Ord a => EdgeList a -> [a]
vertexList = listSetFromList . concatMap f . fromEdgeList
  where f (x,y) = [x,y]
        
standardizeVertices :: (Eq a, Ord a) => EdgeList a -> EdgeList Int
standardizeVertices es = renameVertices (toStandardInt (vertexList es)) es

graphFile :: (Eq a, Ord a) => EdgeList a -> String
graphFile es = 
  let nv = length $ vertexList es
      ne = length $ fromEdgeList es
      se = fromEdgeList $ standardizeVertices es
  in concatMap graphFileLine $ (nv, ne) : se
      
graphFileLine :: (Int, Int) -> String
graphFileLine (x, y) = show x ++ " " ++ show y ++ "\n"

--------

induceSubgraph :: Ord a => Set a -> EdgeList a -> EdgeList a
induceSubgraph vSet = induceSubgraphByTest (`member` vSet)
        
induceSubgraphByTest :: (a -> Bool) -> EdgeList a -> EdgeList a
induceSubgraphByTest test (EdgeList es) = 
  EdgeList $ filter (andTest (test . fst) (test .snd)) es

renameVertices :: (a -> b) -> EdgeList a -> EdgeList b
renameVertices f = EdgeList . map (mapPair f) . fromEdgeList

------------------

arraySquare :: Array Int [Int] -> FullAdj Int
arraySquare es = FullAdj . map squareRow . range . bounds $ es
  where squareRow v = (v, listSetFromList $ within2 es v) 
  
within2 :: Array Int [Int] -> Int -> [Int] 
within2 es v =
  let vs = es ! v
  in vs ++ concatMap (es !) vs

withinN :: Array Int [Int] -> Int -> Int -> [Int] 
withinN es 1 v = es ! v
withinN es n v = 
  let vs = es ! v
  in vs ++ concatMap (withinN es (n-1)) vs
    
  

matrixSquare :: Ord a => FullAdj a -> EdgeList a
matrixSquare = EdgeList . map fst . filter (uncurry intersect . snd) . map transposePairs . allPairs . fromFullAdj
  where 
    transposePairs ((w,x),(y,z)) = ((w,y),(x,z))

allPairs [] = []
allPairs (x:xs) = map ((,) x) xs ++ allPairs xs

complement :: (Eq a, Ord a) => EdgeList a -> EdgeList a
complement es = EdgeList $ asymDiff (allPairs (vertexList es)) (sort (fromEdgeList es))

