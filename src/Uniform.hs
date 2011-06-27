module Uniform 
    (
     UnifNat(..),
     newUnifNat, 
     mappend, 
     mempty,
     addBit,
     bitToUnifNat,
     exchange,
     gcdPlus,
     extractUseful, 
     decision,
     ratioDecision
    )
where

import Bit
import Data.Monoid

data UnifNat a = UnifNat {
      unifValue :: a
    , maxValue  :: a
} deriving Show

newUnifNat value max =
    UnifNat (mod value max) max




--uniform manipulation
{------}{------}{------}{------}{------}{------}{------}{------}{------}{------}

--first argument affects high order bits

addBit (UnifNat x y) bit = UnifNat (doubleIf x bit) (2 * y)

bitToUnifNat False = UnifNat 0 2
bitToUnifNat True  = UnifNat 1 2

instance (Num a) => Monoid (UnifNat a)  where
    mappend (UnifNat x a) (UnifNat y b) = UnifNat (x * b + y) (a * b)
    mempty = UnifNat 0 1


exchange (UnifNat x a) (UnifNat y b) = 
    let (q, r) = quotRem (x * b + y) a
    in (UnifNat q b, UnifNat r a)
    
gcdPlus a b = 
    let c = gcd a b
        d = div a c
        e = div b c
    in (c, d, e)

--use high order bits
extractUseful max (UnifNat a b) = 
    let (usefulSize, stillNeeded, leftoverSize) = gcdPlus max b
        (q, r)                                  = quotRem a leftoverSize
        useful                                  = UnifNat q usefulSize
        leftover                                = UnifNat r leftoverSize
    in (useful, stillNeeded, leftover)




-- decision making
{------}{------}{------}{------}{------}{------}{------}{------}{------}{------}

--decisions are made using high order bits

decision threshold (UnifNat a b) = 
    if a >= threshold
    then (True, UnifNat (a - threshold) (b - threshold))
    else (False, UnifNat a threshold)

ratioDecision numer denom n@(UnifNat _ b) = 
    let threshold = (b * numer) `div` denom
    in decision threshold n
