module Distribution where

import Bitstream
import Uniform

data Interval = Interval Int Int Int
                deriving Show

data Distribution a = Constant a |
                      Bernoulli (Int, Int) (Distribution a) (Distribution a)

instance Functor Distribution where
    fmap f (Constant x) = 
        Constant (f x)

    fmap f (Bernoulli p left right) = 
        Bernoulli p (fmap f left) (fmap f right)


newInterval = Interval 0 1 1

halfInterval (Interval low high denom) True = 
    Interval (low + high) (2*high) (2*denom)

halfInterval (Interval low high denom) False = 
    Interval (2*low) (low + high) (2*denom)


compareInterval (Interval l h d) (p, q) = 
    if q * h <= p * d
    then (Interval (q * l) (q * h) (p * d), LT)
    else if q * l >= p * d
         then (Interval (q * l - p * d) (q * h - p * d) (q * d - p * d), GT)
         else (Interval l h d, EQ)


getValue d = getValue' d newInterval

getValue' (Constant x) _ bs = (x, bs)

getValue' d@(Bernoulli p left right) interval bs =
    case compareInterval interval p of
      (newInterval, LT) -> getValue' left newInterval bs
      (newInterval, GT) -> getValue' right newInterval bs
      (newInterval, EQ) -> let (b, bs') = getBit bs
                           in getValue' d (halfInterval newInterval b) bs'




geometric (p, q) =
    Bernoulli (p, q) (fmap (+ 1) (geometric (p,q))) (Constant 0)
