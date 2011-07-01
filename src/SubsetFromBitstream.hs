module SubsetFromBitstream
       (
         subsetFromBitstream,
         subsetIncrementally,
         subsetIncrementallyM,
       ) where

import UniformGeneration
import SubsetSelection
import Control.Monad.State

subsetFromBitstream n k bs = 
    let max          = choose n k
        (index, bs') = uniform max bs
        subset       = subsetFromUniform n k index
    in (subset, bs')



subsetIncrementallyM n k = state (subsetIncrementally n k)

subsetIncrementally n k bs = subsetInc n k [] mempty bs
    where subsetInc _ 0 subset _    bs = (subset, bs)

          subsetInc n k subset unif bs = 
              let (_, n', k')        = gcdPlus n k
                  (d, leftover, bs') = efficientDecision (n' - k') n' unif bs
              in if d
                 then subsetInc (n - 1) (k - 1) ((n - 1) : subset) leftover bs'
                 else subsetInc (n - 1) k subset leftover bs'
