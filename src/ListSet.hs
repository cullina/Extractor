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

data SP a b = SP !a !b

toPair (SP x y) = (x, y)

fromPair (x, y) = SP x y

mapSSnd f (SP x y) = SP x (f y)

data SL a = SNil
          | SCons !a !(SL a)
            
data LL a = LNil
          | LCons !a (LL a)
            
fromSL :: SL a -> [a]            
fromSL SNil = []
fromSL (SCons x xs) = x : fromSL xs

fromLL :: LL a -> [a]            
fromLL LNil = []
fromLL (LCons x xs) = x : fromLL xs

fromList [] = LNil
fromList (x:xs) = LCons x (fromList xs)

mapS :: (a -> b) -> SL a -> SL b
mapS _ SNil = SNil
mapS f (SCons x xs) = SCons (f x) (mapS f xs)

mapL :: (a -> b) -> LL a -> LL b
mapL _ LNil = LNil
mapL f (LCons x xs) = LCons (f x) (mapL f xs)

toS :: (a,b) -> SP a (SL b)
toS (x,y) = SP x (SCons y SNil)

fromS :: SP a (SL b) -> (a, [b])
fromS (SP x y) = (x, fromSL y)

listSetFromList :: Ord a => [a] -> [a]
listSetFromList = fromSL . listSetFromListS . fromList

listSetFromListS :: Ord a => LL a -> SL a
listSetFromListS = mergeAll mergeSets . mapL (flip SCons SNil)

listMapFromList :: (Ord a, Ord b) => [(a,b)] -> [(a,[b])]
listMapFromList = map fromS . fromSL . listMapFromListS . fromList . map toS
    
listMapFromListS :: (Ord a, Ord b) => LL (SP a (SL b)) -> SL (SP a (SL b))
listMapFromListS = listMapUnion . mapL (flip SCons SNil)
    
listMapUnion :: (Ord a, Ord b) => LL (SL (SP a (SL b))) -> SL (SP a (SL b))
listMapUnion = mergeAll mergeMaps

                   
mergeAll :: (SL a -> SL a -> SL a) -> LL (SL a) -> SL a
mergeAll _     LNil           = SNil
mergeAll _     (LCons x LNil) = x
mergeAll merge xs             = mergeAll merge (mergePairs merge xs)
    
mergePairs :: (SL a -> SL a -> SL a) -> LL (SL a) -> LL (SL a)
mergePairs merge (LCons x (LCons y zs)) = LCons (merge x y) (mergePairs merge zs)
mergePairs _     zs                     = zs

mergeSets :: Ord a => SL a -> SL a -> SL a
mergeSets xx@(SCons x xs) yy@(SCons y ys) = 
  case compare x y of 
    EQ -> SCons x (mergeSets xs ys)
    LT -> SCons x (mergeSets xs yy)
    GT -> SCons y (mergeSets xx ys)
mergeSets xx SNil = xx
mergeSets SNil yy = yy

mergeMaps :: (Ord a, Ord b) => SL (SP a (SL b)) -> SL (SP a (SL b)) -> SL (SP a (SL b))
mergeMaps xx@(SCons (SP x fx) xs) yy@(SCons (SP y fy) ys) = 
  case compare x y of 
    EQ -> SCons (SP x (mergeSets fx fy)) (mergeMaps xs ys)
    LT -> SCons (SP x fx)              (mergeMaps xs yy)
    GT -> SCons (SP y fy)              (mergeMaps xx ys)
mergeMaps xx SNil = xx
mergeMaps SNil yy = yy

