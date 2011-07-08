module SubsetSelection 
       (
         choose,
         subsetFromInteger,
         subsetFromUniform,
         subsetDist,
         getSubset,
         subsetToIndex
       )
where

import Uniform(newUnifNat, ratioDecision)
import Distribution

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

subsetFromUniform n k = subsetFromInteger' n k []

subsetFromInteger' _ 0 subset _ = subset

subsetFromInteger' n k subset index =
    let (d, leftover) = ratioDecision (n - k) n index
    in if d
       then subsetFromInteger' (n - 1) (k - 1) ((n - 1) : subset) leftover 
       else subsetFromInteger' (n - 1) k subset leftover

--

subsetToIndex = subsetToIndex' (0, 1) 0 0

subsetToIndex' index _ _ [] = index

subsetToIndex' (index, max) n k (x:xs) = 
    if x == n
    then let diff = max * (n - k) `div` (k + 1)
         in subsetToIndex' (index + diff, max + diff) (n + 1) (k + 1) xs
    else subsetToIndex' (index, max * (n + 1) `div` (n - k + 1)) (n + 1) k (x:xs)





subsetDist _ 0 = Constant []

subsetDist n k = 
  Bernoulli (k, n) (fmap (n :) (subsetDist (n - 1) (k - 1))) (subsetDist (n - 1) k)


-- index list to subset

subsetToBitList subset = marker 0 subset
    where marker _ [] = []
          marker n (k:ks) = 
              if n == k
              then True  : marker (n + 1) ks
              else False : marker (n + 1) (k:ks)
    

getSubset set = 
    map fst . filter snd . zip set . subsetToBitList

