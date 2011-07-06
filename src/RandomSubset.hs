module RandomSubset 
       (
         subsetIncrementally
       )
       where

import RandomUniform


subsetIncrementally n k = subsetInc n k mempty
    where subsetInc _ 0 _    = Done []
          
          subsetInc n k unif = f =<< efficientDecision (n - k) n unif
          
          f (d, leftover) = 
            if d
            then (n - 1) <:> subsetInc (n - 1) (k - 1) leftover
            else subsetInc (n - 1) k leftover

          x <:> xs = liftM (x :) xs 
          
