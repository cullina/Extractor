module RandomSubset 
       (
         subsetIncrementally,
         subsetViaDist,
         module SubsetSelection
       )
       where

import SubsetSelection
import RandomUniform
import RandomDistribution
import Control.Monad(liftM)
import Data.Ratio((%))


subsetIncrementally n k = sI n k mempty
    where sI 0 _ _    = Done []
          sI n k unif = f =<< efficientDecision (k % n) unif
          
          f (True,  leftover) = True  <:> sI (n - 1) (k - 1) leftover
          f (False, leftover) = False <:> sI (n - 1) k leftover

          x <:> xs = liftM (x :) xs 

subsetViaDist n k = uniformMethod $ subsetDist n k