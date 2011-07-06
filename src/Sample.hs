module Sample where

import Histogram
import RandomValue
import Control.Monad(replicateM)
import System.Random(mkStdGen, randoms)

generateSamples rv n seed = 
    let bs = randoms $ mkStdGen seed
        nDist = replicateM n rv
    in treeHistogram . fst $ fromList nDist bs
