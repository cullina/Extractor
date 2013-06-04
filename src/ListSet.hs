module ListSet where

import Util((...))
import Data.List(sort)

removeSubsets :: Ord a => [[a]] -> [[a]]
removeSubsets [] = []
removeSubsets (x:xs) = x : removeSubsets (filter (not . contains x) xs)

add :: Ord a => a -> [a] -> [a]
add x [] = [x]
add x (y:ys) = 
  case compare x y of
    LT -> x : y : ys
    EQ -> y : ys
    GT -> y : add x ys

merge :: Ord a => [a] -> [a] -> [(Ordering, a)]
merge [] ys         = map ((,) GT) ys
merge xs []         = map ((,) LT) xs
merge (x:xs) (y:ys) = 
  case compare x y of
    LT -> (LT,x) : merge xs (y:ys)
    EQ -> (EQ,x) : merge xs ys
    GT -> (GT,y) : merge (x:xs) ys

union :: Ord a => [a] -> [a] -> [a]
union = map snd ... merge

intersection :: Ord a => [a] -> [a] -> [a]
intersection = (map snd . filter ((EQ ==) . fst)) ... merge

intersect :: Ord a => [a] -> [a] -> Bool
intersect = (not . null) ... intersection

symDiff :: Ord a => [a] -> [a] -> [a]
symDiff = (map snd . filter ((EQ /=) . fst)) ... merge

asymDiff :: Ord a => [a] -> [a] -> [a]
asymDiff = (map snd . filter ((LT ==) . fst)) ... merge

contains :: Ord a => [a] -> [a] -> Bool
contains = null ... flip asymDiff

listSetFromList2 :: Ord a => [a] -> [a]
listSetFromList2 = runs . sort
  where 
    runs []  = []
    runs [x] = [x] 
    runs (x:y:zs)
      | x == y    = runs (y:zs)
      | otherwise = x : runs (y:zs)
                    

fastHist :: Ord a => [a] -> [(a,Int)]
fastHist = h . sort
  where
    h []     = []
    h (x:xs) = g (x,1) xs
    g t []   = [t]
    g (x,n) (y:ys)
      | x == y    = g (x, n+1) ys
      | otherwise = (x,n) : g (y,1) ys

listSetFromList :: Ord a => [a] -> [a]
listSetFromList = mergeAll mergeSets . map return
                   
mergeAll :: ([a] -> [a] -> [a]) -> [[a]] -> [a]
mergeAll _     []  = []
mergeAll _     [x] = x
mergeAll merge xs  = mergeAll merge (mergePairs merge xs)
    
mergePairs :: ([a] -> [a] -> [a]) -> [[a]] -> [[a]]
mergePairs merge (x : y : zs) = merge x y : mergePairs merge zs
mergePairs _     zs           = zs

mergeSets :: Ord a => [a] -> [a] -> [a]
mergeSets xx@(x:xs) yy@(y:ys) = 
  case compare x y of 
    EQ -> x : mergeSets xs ys
    LT -> x : mergeSets xs yy
    GT -> y : mergeSets xx ys
mergeSets xx [] = xx
mergeSets [] yy = yy

----------------------
--ListMap

mapSubset :: Ord a => (b -> b) -> [a] -> [(a,b)] -> [(a,b)]
mapSubset _ _  [] = []
mapSubset f xs (y:ys) = 
  y' : mapSubset f xs' ys
  where
    (xs', y') = g xs y
    
    g []     (y, z) = ([], (y, z))
    g (x:xs) (y, z) = 
      case compare x y of
        LT -> g xs (y, z)
        EQ -> (xs, (y, f z))
        GT -> (x:xs, (y, z))

  