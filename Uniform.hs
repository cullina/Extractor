module Uniform 
    (
     UnifNat(..),
     newUnifNat, 
     mergeUniforms, 
     identityUnifNat,
     addBit,
     gcdPlus,
     extractUseful, 
     decision,
     ratioDecision
    )
where

import Bit
import Bitstream

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

mergeUniforms (UnifNat a b) (UnifNat c d) = UnifNat (a * d + c) (b * d)

identityUnifNat = UnifNat 0 1

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

decision (UnifNat a b) threshold = 
    if a >= threshold
    then (True, UnifNat (a - threshold) (b - threshold))
    else (False, UnifNat a threshold)

ratioDecision n@(UnifNat a b) numer denom = 
    let threshold = (b * numer) `div` denom
    in decision n threshold



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

weightedViaUniform unifSource w@(Weighted total weights) bs =
    mapFst (lookupValue w) $ unifSource total bs