module Sample where

import Distribution
import Histogram
import Bitstream
import Control.Monad(replicateM)

generateSamples dist n seed = 
    let bs = stdBitstream seed
        nDist = replicateM n (getValue dist)
        (samples, _) = useBitstream nDist bs
    in treeHistogram samples
