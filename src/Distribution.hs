module Distribution where

import Uniform(gcdPlus)

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

newWeighted = fst . foldr addElem (Constant undefined, 0)
  where addElem (x, weight) (dist, totalWeight) =
          let sum = weight + totalWeight
              (_, p, q) = gcdPlus weight sum
          in (Bernoulli (p, q) (Constant x) dist, sum) 

    



geometric (p, q) =
    Bernoulli (p, q) (fmap (+ 1) (geometric (p,q))) (Constant 0)
