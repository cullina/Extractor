module Sample where

import Distribution
import Histogram
import Bitstream

generateSamples dist n seed = 
    let bs = stdBitstream seed
        (samples, bs') = generateSamples' dist n [] bs 
    in (treeHistogram samples, bs')

generateSamples' _    0 samples bs = (samples, bs)

generateSamples' dist n samples bs =
    let (x, bs') = getValue dist bs
    in generateSamples' dist (n-1) (x:samples) bs'