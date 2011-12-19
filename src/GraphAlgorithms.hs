module GraphAlgorithms where

import Graph
       

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