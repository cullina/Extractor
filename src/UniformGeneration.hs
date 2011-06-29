module UniformGeneration  
       (
         uniform,
         rejectUniform,
         fastRejectUniform,
         recycleUniform,
         biasedBit,
         uniformViaReal,
         randomDecision,
         efficientDecision,
         efficientDecision2,
         module Uniform
       )where

import Bit
import Bitstream
import Uniform

--Uniform generation

uniform = recycleUniform

--simple rejection sampling
rejectUniform max = rejectUniform' max (maxInBits max)

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

popPush (_:ms) xs bs = 
    let (b, bs') = getBit bs
    in  popPush ms (b:xs) bs'


--recycle rejected portion of interval
recycleUniform max = recycleUniform' max mempty

recycleUniform' max u bs =
    if maxValue u < max
    then let (b, bs') = getBit bs
         in recycleUniform' max (addBit u b) bs'
    else case decision max u of
           (False, u') -> (u', bs)
           (True, u')  -> recycleUniform' max u' bs


-- False with probability num/den

biasedBit _ 0 bs = (True, bs)

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
               
               
-- more uniform generation
{------}{------}{------}{------}{------}{------}{------}{------}{------}{------}

uniformFromRecycle max randInt bs = 
    let (useful, stillNeeded, leftover) = extractUseful max randInt
        (newInt, bs')                   = uniform stillNeeded bs
        merged                          = newInt `mappend` useful
    in (merged, leftover, bs')   


{------}{------}{------}{------}{------}{------}{------}{------}{------}{------}
    
randomDecision threshold max bs =
    let (randInt, bs') = uniform max bs
        (d, leftover)  = decision threshold randInt
    in (d, leftover, bs')

efficientDecision threshold max randInt bs = 
    let (merged, leftover, bs') = uniformFromRecycle max randInt bs
        (d, leftover2)          = decision threshold merged
        leftover3               = leftover2 `mappend` leftover --preserve bit ordering
    in (d, leftover3, bs')

efficientDecision2 threshold max n@(UnifNat _ b) bs =
    let (_, stillNeeded, leftoverSize) = gcdPlus max b
        (newInt, bs')                  = uniform stillNeeded bs
        merged                         = newInt `mappend` n
        (d, leftover)                  = decision (threshold * leftoverSize) merged
    in (d, leftover, bs')
    