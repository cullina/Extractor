module Distribution where

import Util

data Interval a = Interval a a a
                deriving Show

data Distribution i a = Constant a |
                      Bernoulli (i, i) (Distribution i a) (Distribution i a)

instance Functor (Distribution i) where
    fmap f (Constant x) = 
        Constant (f x)

    fmap f (Bernoulli p left right) = 
        Bernoulli p (fmap f left) (fmap f right)


newInterval :: (Integral a) => Interval a

newInterval = Interval 0 1 1

halfInterval (Interval low high denom) True = 
    Interval (low + high) (2*high) (2*denom)

halfInterval (Interval low high denom) False = 
    Interval (2*low) (low + high) (2*denom)


compareInterval (Interval l h d) (p, q)
  | q * h <= p * d  = (LT, Interval (q * l) (q * h) (p * d))
  | q * l >= p * d  = (GT, Interval (q * l - p * d) (q * h - p * d) (q * d - p * d))
  | otherwise       = (EQ, Interval l h d)



                  

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

weightedViaUniform unifSource w@(Weighted total _) =
    mapFst (lookupValue w) $ unifSource total
    



geometric (p, q) =
    Bernoulli (p, q) (fmap (+ 1) (geometric (p,q))) (Constant 0)
