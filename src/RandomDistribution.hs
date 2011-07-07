module RandomDistribution 
       (
         intervalMethod,
         uniformMethod,
         intervalMethod2,
         uniformMethod2,
         module Distribution
       ) where

import Distribution
import RandomValue
import RandomUniform
import Util

intervalMethod d = iM (d newInterval)
  where iM (Constant x) _  = Done x

        iM d@(Bernoulli p left right) interval =
          case compareInterval interval p of
            (LT, newInterval) -> iM left newInterval
            (GT, newInterval) -> iM right newInterval
            (EQ, newInterval) -> NotDone $ \b -> 
                                   iM d (halfInterval newInterval b)

splitInterval p interval =
  case compareInterval interval p of
    (LT, newInterval) -> Done (False, newInterval)
    (GT, newInterval) -> Done (True,  newInterval)
    (EQ, newInterval) -> NotDone $ \b -> splitInterval p (halfInterval newInterval b)


getValue decision startState dist = gV (dist, startState)
  where gV (Constant x,      _    ) = Done x
        gV (Bernoulli p l r, state) = gV . mapFst (f r l) =<< decision p state
        
        f r l b = if b then r else l

uniformMethod2 = getValue efficientDecision mempty

intervalMethod2 = getValue splitInterval newInterval

uniformMethod d = uM (d, mempty)
  where uM (Constant x,      _   ) = Done x
        uM (Bernoulli p l r, unif) = uM . mapFst (f r l) =<< efficientDecision p unif
        
        f r l b = if b then r else l

