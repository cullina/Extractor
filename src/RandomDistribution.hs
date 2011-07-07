module RandomDistribution 
       (
         intervalMethod,
         uniformMethod,
         module Distribution
       ) where

import Distribution
import RandomValue
import RandomUniform
import Util


splitInterval p interval =
  case compareInterval interval p of
    (LT, newInterval) -> Done (False, newInterval)
    (GT, newInterval) -> Done (True,  newInterval)
    (EQ, newInterval) -> NotDone $ \b -> splitInterval p (halfInterval newInterval b)


getValue decision startState dist = gV (dist, startState)
  where gV (Constant x,      _    ) = Done x
        gV (Bernoulli p l r, state) = gV . mapFst (f r l) =<< decision p state
        
        f r l b = if b then r else l

uniformMethod = getValue efficientDecision mempty

intervalMethod = getValue splitInterval newInterval



