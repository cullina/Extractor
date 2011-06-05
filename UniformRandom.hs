module UniformRandom where

import Bit
import Bitstream
import Uniform
import RandomValue

-- Implement some of the algorithms from Uniform using the new type

--simple rejection sampling
rejectUniform max = 
    composeRValues (rU max) (popPush (maxInBits max) [])
    where rU max = NotDone $ \bs -> 
              let try = bitsToInt (reverse bs)
              in if try < max
                 then Done (UnifNat try max)
                 else rU max

--fail fast rejection sampling
fastRejectUniform max = 
    fmap (\x -> UnifNat (bitsToInt (reverse x)) max) (fRU mib mib [])
    where mib               = maxInBits max
          fRU _   []     xs = Done xs
          fRU mib (m:ms) xs = NotDone $ \b -> 
              if m
              then if b
                   then fRU mib ms (b:xs)   {- append and continue -}
                   else popPush ms (b:xs)                  {- append, continue without further checks -}
              else if b
                   then fRU mib mib []      {- discard and restart -}
                   else fRU mib ms (b:xs)   {- append and continue -}
         
         
popPush [] xs = Done xs

popPush (m:ms) xs = NotDone $ \b -> popPush ms (b:xs)



--recycle rejected portion of interval
recycleUniform max = rU max identityUnifNat
    where rU max u =
              if maxValue u < max
              then NotDone $ \b -> rU max (addBit u b)
              else case decision u max of
                    (False, u') -> Done u'
                    (True, u')  -> rU max u'

-- False with probability num/den

biasedBit den 0 = Done True

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
    (uVR max denom) =<< (popPush mib [])
    where mib      = maxInBits max
          denom    = 2 ^ length mib                    
          uVR max denom x = 
              let try      = bitsToInt (reverse x)     
                  (q, r)   = quotRem (try * max) denom 
                  denomMinusR = denom - r
                  f b = if b
                        then (UnifNat (q+1) max)
                        else (UnifNat  q    max)
              in if max <= denomMinusR
                 then Done (UnifNat q max)
                 else fmap f (biasedBit max denomMinusR) 
            
vnUnbias :: RValue Bool Bool
            
vnUnbias = g Nothing 
    where g Nothing  = NotDone $ \b -> g (Just b)
          g (Just a) = NotDone $ \b -> if a == b then g Nothing else Done a