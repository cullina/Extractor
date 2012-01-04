module ListSet where

import Util((...))
import Data.List(sort)

removeSubsets :: Ord a => [[a]] -> [[a]]
removeSubsets [] = []
removeSubsets (x:xs) = x : removeSubsets (filter (not . contains x) xs)

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

listSetFromList :: Ord a => [a] -> [a]
listSetFromList = runs . sort
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
