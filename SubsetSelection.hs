module SubsetSelection 
    (
     choose,
     subsetFromInteger,
     subsetFromBitstream,
     subsetIncrementally,
     subsetIncrementallyM,
     getSubset
    )
where

import Uniform
import UniformGeneration
import Data.List(foldl')
import Control.Monad.State


-- Compute binomial coefficients

choose n k = if (2 * k > n)
             then choose' n (n - k)
             else choose' n k

choose' n 0 = 1
choose' 0 k = 0
choose' n k = (choose' (n-1) (k-1)) * n `div` k


--Uniform subset selection



--index ranges from [0 , nCk)

subsetFromInteger n k index = 
    subsetFromInteger' n k [] (newUnifNat index (choose n k))

subsetFromInteger' n 0 subset index = subset

subsetFromInteger' n k subset index =
    let (d, leftover) = ratioDecision index (n - k) n
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
    where subsetInc n 0 subset unif bs = (subset, bs)

          subsetInc n k subset unif bs = 
              let (g, n', k')        = gcdPlus n k
                  (d, leftover, bs') = efficientDecision (n' - k') n' unif bs
              in if d
                 then subsetInc (n - 1) (k - 1) ((n - 1) : subset) leftover bs'
                 else subsetInc (n - 1) k subset leftover bs'
        
        

--

subsetToIndex subset = subsetToIndex' (0, 1) 0 0 subset

subsetToIndex' index n k [] = index

subsetToIndex' (index, max) n k (x:xs) = 
    if x == n
    then let diff = max * (n - k) `div` (k + 1)
         in subsetToIndex' (index + diff, max + diff) (n + 1) (k + 1) xs
    else subsetToIndex' (index, max * (n + 1) `div` (n - k + 1)) (n + 1) k (x:xs)




subsetToBitList subset = marker 0 subset
    where marker n [] = []
          marker n (k:ks) = 
              if n == k
              then True  : marker (n + 1) ks
              else False : marker (n + 1) (k:ks)
    
-- index list to subset


getSubset set = 
    map fst . filter snd . zip set . subsetToBitList

