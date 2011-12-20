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
partitionNeighbors ns = partition . pN ns
  where
    pN ns [] = []
    pN [] es = map ((,) False) es
    pN (n:ns) ((x,ys):es) =
      case compare n x of 
        LT -> pN ns ((x,ys):es)
        EQ -> (True, (x,ys))  : pN ns es
        GT -> (False, (x,ys)) : pN (n:ns) es
      

partitionNeighbors2 :: Ord a => [a] -> [(a,[a])] -> ( [(a,[a])] , [(a,[a])] )
partitionNeighbors2 = pN
  where
    pN ns [] = ([],[])
    pN [] es = ([],es)
    pN (n:ns) ((x,ys):es) =
      case compare n x of 
        LT -> pN ns ((x,ys):es)
        EQ -> mapFst ((x,ys) :) (pN ns es)
        GT -> mapSnd ((x,ys) :) (pN (n:ns) es)



findAll :: Ord a => ([a] -> [(a,[a])] -> [(a,[a])]) -> [(a,[a])] -> [[a]]
findAll f [] = [[]]
findAll f ((x,ys):g) = 
  map (x :) (findAll f (f ys g)) ++ findAll f g
  
allCliques :: Ord a => [(a,[a])] -> [[a]]
allCliques = removeSubsets . findAll keepNeighbors

allIndepSets :: Ord a => [(a,[a])] -> [[a]]
allIndepSets = removeSubsets . findAll removeNeighbors

maxCliqueSize :: Ord a => Int -> [(a,[a])] -> Int
maxCliqueSize n = maximum . take n . map length . removeSubsets . findAll keepNeighbors

maxIndepSets :: Ord a => FwdAdj a -> [[a]]
maxIndepSets = f . fromFwdAdj 
  where 
    f []         = [[]]
    f ((x,ys):g) = 
      let is = map (x :) (f (removeNeighbors ys g))
      in if length ys > 1
         then is ++ f g
         else is
          
          
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
              
          
maxCliques2 :: Ord a => FwdAdj a -> [[a]]
maxCliques2 = f . fromFwdAdj
  where
    f []         = [[]]
    f ((x,ys):g) = 
      let kept = keepNeighbors ys g
          is = map (x :) (f kept)
      in if length g - length kept > 1
         then is ++ f g
         else is
              
maxCliques3 :: Ord a => FwdAdj a -> [[a]]
maxCliques3 = f . fromFwdAdj
  where
    f []         = [[]]
    f ((x,ys):g) = 
      let kept = keepNeighbors ys g
          is = map (x :) (f kept)
      in if length (removeNeighbors ys g) > 1
         then is ++ f g
         else is
              
maxCliques4 :: Ord a => FwdAdj a -> [[a]]
maxCliques4 = f . fromFwdAdj
  where
    f []         = [[]]
    f ((x,ys):g) = 
      let (kept, rest) = partitionNeighbors2 ys g
          is = map (x :) (f kept)
      in if length rest > 1
         then is ++ f g
         else is
