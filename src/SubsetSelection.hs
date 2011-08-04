module SubsetSelection 
       (
         choose
       , subsetFromInteger
       , subsetFromUniform
       , subsetDist
       , getSubset
       , subsetToIndex
       , partitionSubsets
       , unpartition
       , indicesToBitList
       , bitListToIndices
       , perm
       ) where

import Uniform
import Distribution
import Data.List(partition)
import Data.Ratio((%))

-- Compute binomial coefficients

choose n k = if 2 * k > n
             then choose' n (n - k)
             else choose' n k

choose' _ 0 = 1
choose' 0 _ = 0
choose' n k = choose' (n-1) (k-1) * n `div` k


--Uniform subset selection

--index ranges from [0 , nCk)

subsetFromInteger n k index = 
    subsetFromUniform n k (newUnifNat (choose n k) index)

subsetFromUniform 0 _ _ = []

subsetFromUniform n k index =
    let (d, leftover) = ratioDecision (k % n) index
        k'            = if d then k - 1 else k
    in d : subsetFromUniform (n - 1) k' leftover

--
subsetToIndex :: (Integral a) => [Bool] -> UnifNat a

subsetToIndex = subsetToIndex' mempty 0 0

subsetToIndex' index _ _ [] = index

subsetToIndex' index n k (x:xs) =
  let (n', k') = if x
                 then (n + 1, k + 1)
                 else (n + 1, k)
  in subsetToIndex' (ratioUndecision (n' % k') (x, index))  n' k' xs


subsetDist _ 0 = Constant []

subsetDist n k = 
  let left  = fmap (True  :) $ subsetDist (n - 1) (k - 1)
      right = fmap (False :) $ subsetDist (n - 1) k
  in Bernoulli (k % n) left right


perm :: (Integral a) => [UnifNat a] -> [a]

perm [] = []
perm [UnifNat 1 0] = [0]
perm f@(UnifNat m _ : _) =
  let split = div m 2
      (bs, xs, ys) = disect f split
  in unpartition bs (map (split +) (perm xs)) (perm ys)



disect us split = 
  let ds = decisions us split
      f = map fst
      g = map snd . filter fst
      h = map snd . filter (not . fst)
  in (f ds, g ds, h ds)


decisions [] _ = []
decisions (u:us) split = 
  let (b, v) = decision split u  
  in (b, v) : decisions us (if b then split else (split - 1))






-- index list to subset

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

unpartition []         _      _      = []
unpartition (False:bs) xs     (y:ys) = y : unpartition bs xs ys
unpartition (False:_ ) _      []     = []
unpartition (True: bs) (x:xs) ys     = x : unpartition bs xs ys
unpartition (True: _ ) []     _      = []
