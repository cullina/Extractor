module Uniform 
    (
     newUnifNat,
     dumbUniform, 
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

import Bitstream

data UnifNat a = UnifNat {
      unifValue :: a
    , maxValue  :: a
} deriving Show

newUnifNat value max =
    UnifNat (mod value max) max

--Uniform generation

dumbUniform max bs = dumbUniform' max (maxInBits max) bs

dumbUniform' max mib bs = 
    let (x, bs') = popPush mib [] bs
        try      = bitsToInt (reverse x)
    in if try < max
       then (UnifNat try max, bs')
       else dumbUniform' max mib bs'

uniform max bs = 
    let mib        = maxInBits max
        (out, bs') = uniform' mib mib [] bs
    in (UnifNat (bitsToInt (reverse out)) max, bs')

uniform' _ [] xs bs = (xs, bs)

uniform' mib (m:ms) xs bs = 
    let (b, bs') = getBit bs 
    in if m
       then if b
            then uniform' mib ms (b:xs) bs'   {- append and continue -}
            else popPush ms (b:xs) bs'        {- append, continue without further checks -}
       else if b
            then uniform' mib mib [] bs'      {- discard and restart -}
            else uniform' mib ms (b:xs) bs'   {- append and continue -}

popPush [] xs bs = (xs, bs)

popPush (m:ms) xs bs = 
    let (b, bs') = getBit bs
    in  popPush ms (b:xs) bs'


--uniform manipulation
{------}{------}{------}{------}{------}{------}{------}{------}{------}{------}

--first argument affects high order bits

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
