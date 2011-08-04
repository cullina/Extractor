module Distribution where

import Data.Ratio

data Distribution i a = Constant a
                      | Bernoulli (Ratio i) (Distribution i a) (Distribution i a)

-- Bernoulli is left with probability p

instance Functor (Distribution i) where
    fmap f (Constant x) = 
        Constant (f x)

    fmap f (Bernoulli p left right) = 
        Bernoulli p (fmap f left) (fmap f right)


instance Monad (Distribution i) where
    return = Constant
    
    (Constant x)      >>= f = f x
    (Bernoulli p l r) >>= f = Bernoulli p (l >>= f) (r >>= f)

                  

-- nonuniform finite support random variables
{------}{------}{------}{------}{------}{------}{------}{------}{------}{------}

newWeighted :: (Integral i) => [(a, i)] -> Maybe (Distribution i a)

newWeighted = fmap fst . foldr (maybeMappend mergeWeighted) Nothing . map (Just . mapFst Constant)

maybeMappend f (Just x) (Just y) = Just (f x y)
maybeMappend _ Nothing x = x
maybeMappend _ x Nothing = x

mapFst f (x, y) = (f x, y)

mergeWeighted :: (Integral i) => (Distribution i a, i) -> (Distribution i a, i) -> (Distribution i a, i)

mergeWeighted (l, lWeight) (r, rWeight) =
  let sum       = lWeight + rWeight
  in (Bernoulli (lWeight % sum) l r, sum)



geometric p =
    Bernoulli p (fmap (+ 1) (geometric p)) (Constant 0)
