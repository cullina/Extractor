module Interval where

import Uniform
import Data.Monoid

data Interval a = Interval a a a
                deriving Show


halfInterval (Interval low high denom) True = 
    Interval (low + high) (2*high) (2*denom)

halfInterval (Interval low high denom) False = 
    Interval (2*low) (low + high) (2*denom)


compareInterval (p, q) (Interval l h d)
  | q * h <= p * d  = (LT, Interval (q * l) (q * h) (p * d))
  | q * l >= p * d  = (GT, Interval (q * l - p * d) (q * h - p * d) (q * d - p * d))
  | otherwise       = (EQ, Interval l h d)


instance (Integral a) => Monoid (Interval a) where
  mempty = fromUniform mempty
  mappend (Interval la ha da) (Interval lb hb db) = 
    Interval (la * db + (ha - la) * lb) (la * db + (ha - la) * hb) (da * db)
    
    
    
    
    
    
fromUniform (UnifNat a x) = Interval x (x + 1) a

bitToInterval = fromUniform . bitToUnifNat

halfInterval2 interval = mappend interval . bitToInterval