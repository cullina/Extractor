module RandomSubset where

import UniformRandom

subsetIncrementally n k = subsetInc n k [] mempty
    where subsetInc _ 0 subset _    = Done subset

          subsetInc n k subset unif = 
              f =<< efficientDecision (n' - k') n' unif
              where (_, n', k')   = gcdPlus n k
                    f (d, leftover) = 
                        if d
                        then subsetInc (n - 1) (k - 1) ((n - 1) : subset) leftover
                        else subsetInc (n - 1)      k             subset  leftover