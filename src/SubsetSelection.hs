module SubsetSelection 
    (
     choose,
     subsetFromInteger,
     subsetFromBitstream,
     subsetIncrementally,
     subsetIncrementallyM,
     getSubset,
     subsetToIndex
    )
where

import UniformGeneration
import Control.Monad.State


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
    subsetFromInteger' n k [] (newUnifNat index (choose n k))

subsetFromInteger' _ 0 subset _ = subset

subsetFromInteger' n k subset index =
    let (d, leftover) = ratioDecision (n - k) n index
    in if d
       then subsetFromInteger' (n - 1) (k - 1) ((n - 1) : subset) leftover 
       else subsetFromInteger' (n - 1) k subset leftover

subsetFromBitstream n k bs = 
    let max          = choose n k
        (index, bs') = uniform max bs
        subset       = subsetFromInteger' n k [] index
    in (subset, bs')



subsetIncrementallyM n k = state (subsetIncrementally n k)

subsetIncrementally n k bs = subsetInc n k [] mempty bs
    where subsetInc _ 0 subset _    bs = (subset, bs)

          subsetInc n k subset unif bs = 
              let (_, n', k')        = gcdPlus n k
                  (d, leftover, bs') = efficientDecision (n' - k') n' unif bs
              in if d
                 then subsetInc (n - 1) (k - 1) ((n - 1) : subset) leftover bs'
                 else subsetInc (n - 1) k subset leftover bs'
        
        

--

subsetToIndex = subsetToIndex' (0, 1) 0 0

subsetToIndex' index _ _ [] = index

subsetToIndex' (index, max) n k (x:xs) = 
    if x == n
    then let diff = max * (n - k) `div` (k + 1)
         in subsetToIndex' (index + diff, max + diff) (n + 1) (k + 1) xs
    else subsetToIndex' (index, max * (n + 1) `div` (n - k + 1)) (n + 1) k (x:xs)




subsetToBitList subset = marker 0 subset
    where marker _ [] = []
          marker n (k:ks) = 
              if n == k
              then True  : marker (n + 1) ks
              else False : marker (n + 1) (k:ks)
    
-- index list to subset


getSubset set = 
    map fst . filter snd . zip set . subsetToBitList

