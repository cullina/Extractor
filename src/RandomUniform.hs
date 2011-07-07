module RandomUniform
       (
         uniform,
         rejectUniform,
         fastRejectUniform,
         recycleUniform,
         biasedBit,
         uniformViaReal,
         randomDecision,
         efficientDecision,
         vnUnbias,
         module Uniform,
         module RandomValue
       )where

import Bit
import Uniform
import RandomValue
import Control.Monad(liftM)

-- Implement some of the algorithms from Uniform using the new type

uniform = recycleUniform

--simple rejection sampling
rejectUniform max = 
    untilSuccess . fmap (maybeUnifNat max . bitsToInt) . pP $ maxInBits max

--fail fast rejection sampling
fastRejectUniform max = 
  fmap (UnifNat max . bitsToInt) . untilSuccess . attempt $ maxInBits max
    where b <:> bs = liftM (liftM (b :)) bs
          attempt []     = Done $ Just []
          attempt (m:ms) = NotDone $ \b -> 
            case (m, b) of
              (True,  True ) -> b <:> attempt ms    {- append and continue -}
              (True,  False) -> liftM Just $ pP ms  {- append, no further checks -}     
              (False, True ) -> Done Nothing        {- discard and restart -}
              (False, False) -> b <:> attempt ms    {- append and continue -}


--recycle rejected portion of interval
recycleUniform max = rU max mempty
    where rU max u =
              if maxValue u < max
              then NotDone $ \b -> rU max (addBit u b)
              else case decision max u of
                    (False, u') -> Done u'
                    (True, u')  -> rU max u'

-- False with probability num/den

biasedBit _ 0 = Done True

biasedBit den num = NotDone $ \b ->
    let num' = 2 * num
    in if num' >= den
       then if b
            then biasedBit den (num' - den)
            else Done b
       else if b
            then Done b
            else biasedBit den num'

-- denom >= max
-- try \in [0, denom)
-- consider try as [try * max, (try + 1) * max) \subset [0, denom * max)
-- find q such that  q * denom <= try * max < (q + 1) * denom
--                   q * denom + r = try * max
-- check for containment in [q * denom, (q + 1) * denom):
-- need  (try + 1) max < (q + 1) * denom
-- q * denom + r + max < q * denom + denom
-- r + max < denom

uniformViaReal max = 
    uVR max denom =<< pP mib
    where mib      = maxInBits max
          denom    = 2 ^ length mib                    
          uVR max denom x = 
              let try      = bitsToInt x
                  (q, r)   = quotRem (try * max) denom 
                  denomMinusR = denom - r
                  f b = if b
                        then UnifNat max (q+1)
                        else UnifNat max  q   
              in if max <= denomMinusR
                 then Done (UnifNat max q)
                 else fmap f (biasedBit max denomMinusR) 

                 
randomDecision threshold max =
    fmap (decision threshold) (uniform max)
            
-- q' * leftoverSize == b * stillNeeded == maxValue (newInt `mappend` n)
efficientDecision (p, q)  n@(UnifNat b _) =
    let (_, p', q')                    = gcdPlus p q
        (_, stillNeeded, leftoverSize) = gcdPlus q' b
        d newInt = decision (p' * leftoverSize) (newInt `mappend` n)
    in fmap d (uniform stillNeeded)

    
    
    
vnUnbias :: RValue Bool Bool
            
vnUnbias = g Nothing 
    where g Nothing  = NotDone $ \b -> g (Just b)
          g (Just a) = NotDone $ \b -> if a == b then g Nothing else Done a