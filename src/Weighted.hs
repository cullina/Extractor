module Weighted where


-- nonuniform finite support random variables
{------}{------}{------}{------}{------}{------}{------}{------}{------}{------}

data Weighted a b = Weighted a [(a,b)]


newWeighted weights = Weighted (sum (map fst weights)) weights


newWeightedInts weights = newWeighted $ zip weights [0..]


lookupValue (Weighted total pxs) n =
    lookupValue' pxs (mod n total)

lookupValue' ((p,x):pxs) n = 
    if p > n
    then x
    else lookupValue' pxs (n-p)
         
lookupValue' [] _ = undefined

weightedViaUniform unifSource w@(Weighted total _) bs =
    mapFst (lookupValue w) $ unifSource total bs
    
mapFst f (x, y) = (f x, y)