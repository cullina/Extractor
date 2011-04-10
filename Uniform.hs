module Uniform 
    (
     newUnifNat,
     uniform, 
     mergeUniforms, 
     identityUnifNat,
     extractUseful, 
     uniformFromRecycle, 
     decision,
     ratioDecision,
     randomDecision,
     efficientDecision
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

--Uniform generation

uniform = recycleUniform

--simple rejection sampling
rejectUniform max bs = rejectUniform' max (maxInBits max) bs

rejectUniform' max mib bs = 
    let (x, bs') = popPush mib [] bs
        try      = bitsToInt (reverse x)
    in if try < max
       then (UnifNat try max, bs')
       else rejectUniform' max mib bs'

--fail fast rejection sampling
fastRejectUniform max bs = 
    let mib        = maxInBits max
        (out, bs') = fastRejectUniform' mib mib [] bs
    in (UnifNat (bitsToInt (reverse out)) max, bs')

fastRejectUniform' _ [] xs bs = (xs, bs)

fastRejectUniform' mib (m:ms) xs bs = 
    let (b, bs') = getBit bs 
    in if m
       then if b
            then fastRejectUniform' mib ms (b:xs) bs'   {- append and continue -}
            else popPush ms (b:xs) bs'        {- append, continue without further checks -}
       else if b
            then fastRejectUniform' mib mib [] bs'      {- discard and restart -}
            else fastRejectUniform' mib ms (b:xs) bs'   {- append and continue -}

popPush [] xs bs = (xs, bs)

popPush (m:ms) xs bs = 
    let (b, bs') = getBit bs
    in  popPush ms (b:xs) bs'


--recycle rejected portion of interval
recycleUniform max bs = recycleUniform' max identityUnifNat bs

recycleUniform' max u bs =
    if maxValue u < max
    then let (b, bs') = getBit bs
         in recycleUniform' max (addBit u b) bs'
    else case decision u max of
           (False, u') -> (u', bs)
           (True, u')  -> recycleUniform' max u' bs


-- False with probability num/den

biasedBit den 0 bs = (True, bs)

biasedBit den num bs = 
    let (b, bs') = getBit bs
        num'     = 2 * num
    in if num' >= den
       then if b
            then biasedBit den (num' - den) bs'
            else (b, bs')
       else if b
            then (b, bs')
            else biasedBit den num' bs'

uniformViaReal max bs = 
    let mib      = maxInBits max
        denom    = 2 ^ length mib
        (x, bs') = popPush mib [] bs
        try      = bitsToInt (reverse x)
        (q, r)   = quotRem (try * max) denom
    in if r + max <= denom
       then (UnifNat q max, bs')
       else let (b, bs'') = biasedBit max (denom - r) bs'
            in if b
               then (UnifNat (q+1) max, bs'')
               else (UnifNat  q    max, bs'')


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

-- more uniform generation
{------}{------}{------}{------}{------}{------}{------}{------}{------}{------}

uniformFromRecycle max randInt bs = 
    let (useful, stillNeeded, leftover) = extractUseful max randInt
        (newInt, bs')                   = uniform stillNeeded bs
        merged                          = mergeUniforms newInt useful
    in (merged, leftover, bs')   

uniformFromRecycle2 max n@(UnifNat a b) bs =
    let (usefulSize, stillNeeded, leftoverSize) = gcdPlus max b
        (newInt, bs')                           = uniform stillNeeded bs
        merged                                  = mergeUniforms newInt n
    in (merged, leftoverSize, bs')


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

randomDecision threshold max bs =
    let (randInt, bs') = uniform max bs
        (d, leftover)  = decision randInt threshold
    in (d, leftover, bs')

efficientDecision threshold max randInt bs = 
    let (merged, leftover, bs') = uniformFromRecycle max randInt bs
        (d, leftover2)          = decision merged threshold
        leftover3               = mergeUniforms leftover2 leftover --preserve bit ordering
    in (d, leftover3, bs')

efficientDecision2 threshold max randInt bs =
    let (merged, leftover, bs') = uniformFromRecycle2 max randInt bs
        (d, leftover2)          = decision merged (threshold * leftover)
    in (d, leftover2, bs')

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