module Interval where

import Uniform
import Data.Monoid
import Data.Ratio

data Interval a = Interval (Ratio a) (Ratio a)                
                deriving Show


halfInterval (Interval low high) True = 
    Interval ((low + high) / 2) high

halfInterval (Interval low high) False = 
    Interval low ((low + high) / 2)


validate (Interval low high) = 
  0 <= low && low <= high && high <= 1

compareInterval r (Interval l h)
  | h <= r    = (LT, Interval (l / r) (h / r))
  | l >= r    = (GT, Interval ((l - r) / (1 - r)) ((h - r) / (1 - r)))
  | otherwise = (EQ, Interval l h)


instance (Integral a) => Monoid (Interval a) where
  mempty = fromUniform mempty
  mappend (Interval la ha) (Interval lb hb) = 
    Interval (la + (ha - la) * lb) (la + (ha - la) * hb)
    
    
    
fromUniform (UnifNat a x) = Interval (x % a) ((x + 1) % a)

bitToInterval = fromUniform . bitToUnifNat

halfInterval2 interval = mappend interval . bitToInterval