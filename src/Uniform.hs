module Uniform 
       (
         UnifNat(..)
       , newUnifNat
       , maybeUnifNat
       , allUnifs
       , mappend
       , mempty
       , addBit
       , bitToUnifNat
       , exchange
       , gcdPlus
       , extractUseful
       , decision
       , ratioDecision
       , ratioUndecision
       ) where

import Data.Monoid
import Data.Ratio

data UnifNat a = UnifNat {
      maxValue :: a
    , unifValue  :: a
} deriving Show

newUnifNat max value =
    UnifNat max (mod value max)
    
maybeUnifNat max value = 
  if value < max && value >= 0
  then Just $ UnifNat max value
  else Nothing


allUnifs max = map (UnifNat max) [0..max-1]


--uniform manipulation
{------}{------}{------}{------}{------}{------}{------}{------}{------}{------}

--first argument affects high order bits

addBit (UnifNat y x) True  = UnifNat (2 * y) (2 * x)
addBit (UnifNat y x) False = UnifNat (2 * y) (2 * x + 1)

bitToUnifNat True  = UnifNat 2 0
bitToUnifNat False = UnifNat 2 1


instance (Integral a) => Monoid (UnifNat a)  where
    mappend (UnifNat a x) (UnifNat b y) = UnifNat (a * b) (x * b + y)
    mempty = UnifNat 1 0


exchange (UnifNat a x) (UnifNat b y) = 
    let (q, r) = quotRem (x * b + y) a
    in (UnifNat b q, UnifNat a r)
    
gcdPlus a b = 
    let c = gcd a b
        d = div a c
        e = div b c
    in (c, d, e)

--use high order bits
extractUseful max (UnifNat b a) = 
    let (usefulSize, stillNeeded, leftoverSize) = gcdPlus max b
        (q, r)                                  = quotRem a leftoverSize
        useful                                  = UnifNat usefulSize q
        leftover                                = UnifNat leftoverSize r
    in (useful, stillNeeded, leftover)




-- decision making
{------}{------}{------}{------}{------}{------}{------}{------}{------}{------}

-- decisions are made using high order bits
-- True w/ prob (threshold / b)

decision threshold (UnifNat b a) = 
    if a < threshold
    then (True, UnifNat threshold a)
    else (False, UnifNat (b - threshold) (a - threshold))

-- True with prob (numer / denom) when `div` is exact
ratioDecision :: (Integral i) => Ratio i -> UnifNat i -> (Bool, UnifNat i)
ratioDecision r n@(UnifNat b _) = 
    let threshold = numerator $ r * (b % 1)    
    in decision threshold n

ratioUndecision r (False, UnifNat b a) = 
  let upper = numerator $ (b % 1) * (1 - r) / r
  in UnifNat (b + upper) a

ratioUndecision r (True, UnifNat b a) = 
  let lower = numerator $ (b % 1) * r / (1 - r)
  in UnifNat (b + lower) (a + lower)
  