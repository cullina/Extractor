module Simplex where

import Data.Ratio
import Data.List(unfoldr,foldl')
import ListSet(union)

data LinEx = LinEx Rational [(Int, Rational)] deriving Show

data Instance = Instance LinEx [(Int, LinEx)] deriving Show

zero = 0 % 1
one = 1 % 1
negOne = (-1) % 1


--LinEx manipulations

mapSnd f (x,y) = (x, f y)

coeff :: Int -> [(Int,Rational)] -> Rational
coeff _ []         = zero
coeff i ((j,c):cs) = 
  if i == j
  then c
  else coeff i cs
       
remove :: Int -> [(Int, a)] -> [(Int, a)]
remove _ [] = []
remove i ((j,x):xs) = 
  if i == j
  then xs
  else (j,x) : remove i xs
       
mulEx :: Rational -> LinEx -> LinEx
mulEx a (LinEx c xs) = LinEx (a * c) (mulList a xs)

mulList :: Rational -> [(Int, Rational)] -> [(Int, Rational)]
mulList a = map (mapSnd (a *))
       
addExs :: LinEx -> LinEx -> LinEx
addExs (LinEx a xs) (LinEx b ys) = LinEx (a + b) (addLists xs ys)
            
addLists :: [(Int, Rational)] -> [(Int, Rational)] -> [(Int, Rational)]
addLists xs [] = xs
addLists [] ys = ys
addLists xx@((i,x):xs) yy@((j,y):ys) = 
  case compare i j of
    EQ -> (i,x+y) : addLists xs ys
    LT -> (i,x) : addLists xs yy
    GT -> (j,y) : addLists xx ys


       
substitute :: (Int,LinEx) -> LinEx -> LinEx
substitute (j, ys) (LinEx a xs) = 
  addExs (LinEx a (remove j xs)) (mulEx (-(coeff j xs)) ys)
     
constraint :: Int -> (Int, LinEx) -> (Int, LinEx, Maybe Rational)
constraint i (j, x@(LinEx a xs)) =
  let c = coeff i xs
  in if c == 0
     then (j, x, Nothing)
     else (j, x, Just (a / c))

     
packingInstance :: [[Int]] -> Instance
packingInstance sets = 
  let buildLinEx :: Rational -> Rational -> [Int] -> LinEx
      buildLinEx c a xs = LinEx c (zip xs (repeat a))
      vars   = foldr union [] sets
      value  = buildLinEx zero one vars
      slacks = zip [-1,-2..] . map (buildLinEx one negOne) $ sets
  in Instance value slacks
     
solve :: Instance -> [Rational]
solve = unfoldr step
  where
    step :: Instance -> Maybe (Rational, Instance)
    step inst@(Instance (LinEx _ vcs) basics) =
      let f i = valueOf . pivot inst . rearrange i $ firstConstraint i basics
      in fmap f (findInc vcs)
    
findInc :: [(Int,Rational)] -> Maybe Int
findInc [] = Nothing
findInc ((i,x):xs) = 
  if x > zero
  then Just i
  else findInc xs
      
    
firstConstraint :: Int -> [(Int,LinEx)] -> (Int,LinEx)
firstConstraint i bs = 
  case foldl' maxShift (0, LinEx zero [], Nothing) $ map (constraint i) bs of
   (_, _, Nothing) -> error "unbounded"
   (i, x, _)       -> (i,x)
  where
    maxShift x@(_,_,a) y@(_,_,b) =
      if b > a
      then y
      else x

rearrange :: Int -> (Int, LinEx) -> (Int, (Int, LinEx))
rearrange i (j, (LinEx c x)) = 
  let x' = mulEx (recip (coeff i x)) . LinEx c $ addLists (remove i x) [(j, negOne)]
  in (j, (i, x'))

pivot :: Instance -> (Int, (Int, LinEx)) -> Instance
pivot (Instance value basics) (j,update) = 
  Instance (substitute update value) (map (mapSnd (substitute update)) $ remove j basics)

valueOf :: Instance -> (Rational, Instance)
valueOf i@(Instance (LinEx v _) _) = (v,i)

