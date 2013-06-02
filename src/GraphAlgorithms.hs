module GraphAlgorithms where

import Graph
import Util(mapFst, mapSnd)
import ListSet(contains, intersect)       
import Data.Foldable(foldrM)

removeNeighbors :: Ord a => [a] -> [(a,[a])] -> [(a,[a])]
removeNeighbors [] es = es
removeNeighbors _  [] = []
removeNeighbors (v:vs) ((x,ys):es) = 
  case compare v x of 
    LT -> removeNeighbors vs ((x,ys):es)
    EQ -> removeNeighbors vs es
    GT -> (x,ys) : removeNeighbors (v:vs) es

keepNeighbors :: Ord a => [a] -> [(a,[a])] -> [(a,[a])]
keepNeighbors [] _  = []
keepNeighbors _  [] = []
keepNeighbors (v:vs) ((x,ys):es) = 
  case compare v x of 
    LT -> keepNeighbors vs ((x,ys):es)
    EQ -> (x,ys) : keepNeighbors vs es
    GT -> keepNeighbors (v:vs) es

partitionNeighbors :: Ord a => [a] -> [(a,[a])] -> ( [(a,[a])] , [(a,[a])] )
partitionNeighbors _  [] = ([],[])
partitionNeighbors [] es = ([],es)
partitionNeighbors (n:ns) ((x,ys):es) =
  case compare n x of 
    LT -> partitionNeighbors ns ((x,ys):es)
    EQ -> mapFst ((x,ys) :) (partitionNeighbors ns es)
    GT -> mapSnd ((x,ys) :) (partitionNeighbors (n:ns) es)


maxCliques :: Ord a => FwdAdj a -> [[a]]
maxCliques = f . fromFwdAdj
  where
    f []         = [[]]
    f ((x,ys):g) = 
      let (kept, rest) = partitionNeighbors ys g
          is = map (x :) (f kept)
      in if length rest > 1
         then is ++ f g
         else is

allMaxCliques :: Ord a => FwdAdj a -> [[a]]
allMaxCliques = f . fromFwdAdj
  where
    f []         = [[]]
    f ((x,ys):g) = 
      let (kept, rest) = partitionNeighbors ys g
          is = map (x :) (f kept)
      in if null rest
         then is   
         else is ++ filter (not . contains (map fst kept)) (f g)


maxIndepSets :: Ord a => FwdAdj a -> [[a]]
maxIndepSets = f . fromFwdAdj
  where
    f []         = [[]]
    f ((x,ys):g) = 
      let (rest, kept) = partitionNeighbors ys g
          is = map (x :) (f kept)
      in if length rest > 1
         then is ++ f g
         else is

allMaxIndepSets :: Ord a => FwdAdj a -> [[a]]
allMaxIndepSets = f . fromFwdAdj
  where
    f []         = [[]]
    f ((x,ys):g) = 
      let (rest, kept) = partitionNeighbors ys g
          is = map (x :) (f kept)
      in if null rest
         then is   
         else is ++ filter (not . contains (map fst kept)) (f g)

allIndepSets :: Ord a => FwdAdj a -> [[a]]
allIndepSets = foldrM f [] . fromFwdAdj
  where
    f :: Ord a => (a, [a]) -> [a] -> [[a]]
    f (u, vs) is = 
      if vs `intersect` (u:is)
      then [is]
      else [u:is, is]      
      
allBigIndepSets :: Ord a => Int -> FwdAdj a -> [[a]]
allBigIndepSets l = filter ((>= l) . length) . allIndepSets 

greedyIndepSet :: Ord a => FwdAdj a -> [a]
greedyIndepSet = head . maxIndepSets

greedyClique :: Ord a => FwdAdj a -> [a]
greedyClique = head . maxCliques
--------------------


allColorings :: Ord a => Int -> FwdAdj a -> [[[a]]]
allColorings k = foldrM f [] . fromFwdAdj
  where
    f :: Ord a => (a, [a]) -> [[a]] -> [[[a]]]
    f (u, vs) colorClasses =
      let cc = partialMap (tryColor u vs) colorClasses
      in if k > length colorClasses
         then ([u]:colorClasses) : cc
         else cc
      
    tryColor :: Ord a => a -> [a] -> [a] -> Maybe [a]
    tryColor u vs cs =
      if vs `intersect` cs
      then Nothing
      else Just (u:cs)

    partialMap :: (b -> Maybe b) -> [b] -> [[b]]
    partialMap _ [] = []
    partialMap f (x:xs) = 
      let rest = map (x :) (partialMap f xs)
      in case f x of 
        Nothing -> rest
        Just y -> (y : xs) : rest
  
