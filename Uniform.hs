module Uniform 
    (
     dumbUniform, 
     uniform, 
     mergeUniforms, 
     extractUseful, 
     uniformFromRecycle, 
     decision,
     randomDecision,
     efficientDecision
    )
where

import Bitstream

--Uniform generation

dumbUniform max bs = dumbUniform' max (maxInBits max) bs

dumbUniform' max mib bs = 
    let (x, bs') = popPush mib [] bs
        try      = bitsToInt (reverse x)
    in if try < max
       then ((try, max), bs')
       else dumbUniform' max mib bs'

uniform max bs = 
    let mib        = maxInBits max
        (out, bs') = uniform' mib mib [] bs
    in ((bitsToInt (reverse out), max), bs')

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

mergeUniforms (a,b) (c,d) = (a * d + c, b * d)


gcdPlus a b = 
    let c = gcd a b
        d = div a c
        e = div b c
    in (c, d, e)

--use high order bits
extractUseful max (a,b) = 
    let (usefulSize, stillNeeded, leftoverSize) = gcdPlus max b
        (q, r)                                  = quotRem a leftoverSize
        useful                                  = (q, usefulSize)
        leftover                                = (r, leftoverSize)
    in (useful, stillNeeded, leftover)

-- more uniform generation
{------}{------}{------}{------}{------}{------}{------}{------}{------}{------}

uniformFromRecycle max randInt bs = 
    let (useful, stillNeeded, leftover) = extractUseful max randInt
        (newInt, bs')                   = uniform stillNeeded bs
        merged                          = mergeUniforms newInt useful
    in (merged, leftover, bs')   

uniformFromRecycle2 max (a, b) bs =
    let (usefulSize, stillNeeded, leftoverSize) = gcdPlus max b
        (newInt, bs')                           = uniform stillNeeded bs
        merged                                  = mergeUniforms newInt (a, b)
    in (merged, leftoverSize, bs')


-- decision making
{------}{------}{------}{------}{------}{------}{------}{------}{------}{------}

--decisions are made using high order bits

decision (a,b) threshold = 
    if a >= threshold
    then (True, (a - threshold, b - threshold))
    else (False, (a, threshold))

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
