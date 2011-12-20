module GraphAlgorithms where

import Graph
import Util(partition, mapFst, mapSnd)
       

removeNeighbors :: Ord a => [a] -> [(a,[a])] -> [(a,[a])]
removeNeighbors [] es = es
removeNeighbors vs [] = []
removeNeighbors (v:vs) ((x,ys):es) = 
  case compare v x of 
    LT -> removeNeighbors vs ((x,ys):es)
    EQ -> removeNeighbors vs es
    GT -> (x,ys) : removeNeighbors (v:vs) es

keepNeighbors :: Ord a => [a] -> [(a,[a])] -> [(a,[a])]
keepNeighbors [] es = []
keepNeighbors vs [] = []
keepNeighbors (v:vs) ((x,ys):es) = 
  case compare v x of 
    LT -> keepNeighbors vs ((x,ys):es)
    EQ -> (x,ys) : keepNeighbors vs es
    GT -> keepNeighbors (v:vs) es

partitionNeighbors :: Ord a => [a] -> [(a,[a])] -> ( [(a,[a])] , [(a,[a])] )
partitionNeighbors ns [] = ([],[])
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

--------------------

