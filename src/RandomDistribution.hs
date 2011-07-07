module RandomDistribution 
       (
         intervalMethod,
         uniformMethod,
         module Distribution
       ) where

import Distribution
import Interval
import RandomValue
import RandomUniform(efficientDecision, mempty)
import Util(mapFst)


splitInterval p interval =
  case compareInterval p interval of
    (LT, newInterval) -> Done (False, newInterval)
    (GT, newInterval) -> Done (True,  newInterval)
    (EQ, newInterval) -> NotDone $ \b -> splitInterval p (halfInterval newInterval b)


getValue decision dist = gV (dist, mempty)
  where gV (Constant x,      _    ) = Done x
        gV (Bernoulli p l r, state) = gV . mapFst (f r l) =<< decision p state
        
        f r l b = if b then r else l

uniformMethod = getValue efficientDecision

intervalMethod = getValue splitInterval



