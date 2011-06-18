module RandomSubset where

import RandomValue
import Uniform
import UniformRandom

subsetIncrementally n k = subsetInc n k [] mempty
    where subsetInc n 0 subset unif = Done subset

          subsetInc n k subset unif = 
              f =<< efficientDecision (n' - k') n' unif
              where (g, n', k')   = gcdPlus n k
                    f (d, leftover) = 
                        if d
                        then subsetInc (n - 1) (k - 1) ((n - 1) : subset) leftover
                        else subsetInc (n - 1)      k             subset  leftover