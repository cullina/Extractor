module Distribution where

import Uniform(gcdPlus)

data Distribution i a = Constant a |
                      Bernoulli (i, i) (Distribution i a) (Distribution i a)

instance Functor (Distribution i) where
    fmap f (Constant x) = 
        Constant (f x)

    fmap f (Bernoulli p left right) = 
        Bernoulli p (fmap f left) (fmap f right)




                  

-- nonuniform finite support random variables
{------}{------}{------}{------}{------}{------}{------}{------}{------}{------}

newWeighted = fst . foldr addElem (Constant undefined, 0)
  where addElem (x, weight) (dist, totalWeight) =
          let sum = weight + totalWeight
              (_, p, q) = gcdPlus weight sum
          in (Bernoulli (p, q) (Constant x) dist, sum) 

    



geometric (p, q) =
    Bernoulli (p, q) (fmap (+ 1) (geometric (p,q))) (Constant 0)
