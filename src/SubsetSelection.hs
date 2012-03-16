module SubsetSelection 
       (
         choose
       , subsetFromInteger
       , subsetFromUniform
       , subsetDist
       , allSubsets
       , allSubsets2
       , allSubsetsOf
       , getSubset
       , subsetToIndex
       , subsetToInteger
       , partitionSubsets
       , unpartition
       , indicesToBitList
       , bitListToIndices
       , allMultinomials
       ) where

import Uniform
import Distribution
import Data.List(partition, unfoldr)
import Data.Ratio((%))
import Control.Applicative((<$>))
import Data.Foldable(foldrM)

-- Compute binomial coefficients

choose :: (Integral a) => a -> a -> a
choose n k = if 2 * k > n
             then choose' n (n - k)
             else choose' n k

choose' _ 0 = 1
choose' 0 _ = 0
choose' n k = choose' (n-1) (k-1) * n `div` k


--Uniform subset selection

--index ranges from [0 , nCk)


subsetFromInteger :: (Integral i) => i -> i -> i -> [Bool] 
subsetFromInteger n k index = 
    subsetFromUniform (n, k) (newUnifNat (choose n k) index)

subsetFromUniform :: (Integral i) => (i, i) -> UnifNat i -> [Bool] 
subsetFromUniform = curry (unfoldr sFU)
  where sFU ((0, _), _) = Nothing
        sFU ((n, k), index) = 
          let (d, leftover) = ratioDecision (k % n) index
              k'            = if d then k - 1 else k
          in Just (d, ((n - 1, k'), leftover))

--        
subsetToIndex :: (Integral a) => [Bool] -> ((a, a), UnifNat a)

subsetToIndex = foldr sTI ((0, 0), mempty)
  where sTI x ((n, k), i) =
          let k' = if x then k + 1 else k
              n' = n + 1
              i' = ratioUndecision (k' % n') (x, i)
          in ((n', k'), i')

subsetToInteger :: (Integral a) => [Bool] -> a
subsetToInteger = unifValue . snd . subsetToIndex

subsetDist :: (Integral a) => a -> a -> Distribution a [Bool]
subsetDist n k
  | n == 0    = Constant []
  | k == 0    = right
  | k == n    = left 
  | otherwise =  Bernoulli (k % n) left right
    where left  = (True :)  <$> subsetDist (n - 1) (k - 1)
          right = (False :) <$> subsetDist (n - 1) k
  
allSubsets :: (Integral a) => a -> a -> [[Bool]]
allSubsets n k
  | k < 0     = []
  | k > n     = []
  | otherwise = allShuffles (replicate (fromIntegral k) True) (replicate (fromIntegral (n - k)) False)
          
allShuffles :: [a] -> [a] -> [[a]]
allShuffles xs [] = [xs]
allShuffles [] ys = [ys]
allShuffles (x:xs) (y:ys) =
  ((x :) <$> allShuffles xs (y:ys)) ++ 
  ((y :) <$> allShuffles (x:xs) ys)
     
allMultinomials :: [Int] -> [[Int]]
allMultinomials ks = 
  let f (a, k) = allShuffles (replicate k a)
  in if all (0 <=) ks
     then foldrM f [] $ zip [0..] ks
     else []
    


allSubsets2 :: (Integral a) => a -> a -> [[Bool]]
allSubsets2 n = support . subsetDist n

allSubsetsOf k set = map (getSubset set) $ allSubsets (length set) k

-- index list to subset
getSubset :: [a] -> [Bool] -> [a]
getSubset set = 
    map fst . filter snd . zip set

bitListToIndices :: (Integral a) => [Bool] -> [a]
bitListToIndices = getSubset [0..]


indicesToBitList = marker 0
    where marker _ [] = []
          marker n (k:ks) = 
              if n == k
              then True  : marker (n + 1) ks
              else False : marker (n + 1) (k:ks)
    

partitionSubsets set = 
    pairMap (map fst) . partition snd . zip set
    
pairMap f (x, y) = (f x, f y)    

unpartition :: [Bool] -> [a] -> [a] -> [a]
unpartition []         _      _      = []
unpartition (False:bs) xs     (y:ys) = y : unpartition bs xs ys
unpartition (False:_ ) _      []     = []
unpartition (True: bs) (x:xs) ys     = x : unpartition bs xs ys
unpartition (True: _ ) []     _      = []

