module RandomSubset 
       (
         subsetIncrementally,
         module SubsetSelection
       )
       where

import SubsetSelection
import RandomUniform
import Control.Monad(liftM)

subsetIncrementally n k = sI n k mempty
    where sI _ 0 _    = Done []
          sI n k unif = f =<< efficientDecision (n - k, n) unif
          
          f (True,  leftover) = (n - 1) <:> sI (n - 1) (k - 1) leftover
          f (False, leftover) = sI (n - 1) k leftover

          x <:> xs = liftM (x :) xs 
