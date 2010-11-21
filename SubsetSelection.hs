module SubsetSelection 
    (
     choose,
     chooseList,
     subsetFromInteger,
     subsetFromBitstream,
     subsetIncrementally,
     indicesToSubset
    )
where

import Uniform
import Data.List(foldl')


-- Compute binomial coefficients

choose n k = if (2 * k > n)
             then choose' n (n - k)
             else choose' n k

choose' n 0 = 1
choose' 0 k = 0
choose' n k = (choose' (n-1) (k-1)) * n `div` k


chooseList n k = let (a,b) = chooseList' n k
                 in a:b

chooseList' n 0 = (1,[])
chooseList' 0 k = (0, [])
chooseList' n k = let (a,b) = chooseList' (n-1) (k-1)
                  in (a * n `div` k, a:b)


--Uniform subset selection



--index ranges from [0 , nCk)

subsetFromInteger n k index = 
    let nCk    = choose n k
        index' = rem index nCk
    in subsetFromInteger' n k [] (index', nCk)

subsetFromInteger' n 0 subset index = subset

subsetFromInteger' n k subset index =
    let nCk = snd index
        threshold = div (nCk * (n - k)) n
        (d, leftover) = decision index threshold
    in if d
       then subsetFromInteger' (n - 1) (k - 1) (n : subset) leftover 
       else subsetFromInteger' (n - 1) k subset leftover

subsetFromBitstream n k bs = 
    let max          = choose n k
        (index, bs') = uniform max bs
        subset       = subsetFromInteger' n k [] index
    in (subset, bs')


subsetIncrementally n k bs = subsetInc n k [] (0,1) bs

subsetInc n 0 subset unif bs = (subset, bs)

subsetInc n k subset unif bs = 
    let g  = gcd n k
        n' = div n g
        k' = div k g
        (d, leftover, bs') = efficientDecision (n' - k') n' unif bs
    in if d
       then subsetInc (n - 1) (k - 1) (n : subset) leftover bs'
       else subsetInc (n - 1) k subset leftover bs'
        
        

--

subsetToIndex subset = subsetToIndex' (0, 1) 0 0 subset

subsetToIndex' index n k [] = index

subsetToIndex' (index, max) n k (x:xs) = 
    if x == n + 1
    then let diff = max * (n - k) `div` (k + 1)
         in subsetToIndex' (index + diff, max + diff) (n + 1) (k + 1) xs
    else subsetToIndex' (index, max * (n + 1) `div` (n - k + 1)) (n + 1) k (x:xs)




-- index list to subset


indicesToSubset indices set = 
    let maxIndex = length set - 1                                           
        (a,b,c)  = foldl' picker (0, indices, []) set
    in c 

picker x@(n, [], picked) elem = x

picker (n, k:ks, picked) elem = 
    if n == k
    then (n + 1, ks, elem:picked)
    else (n + 1, k:ks, picked)

