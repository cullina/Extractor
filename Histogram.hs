module Histogram ( histogram ) where

import Data.List(foldl')

--histogram

histogramUpdate [] 0 = [1]

histogramUpdate [] n = 0 : histogramUpdate [] (n - 1)

histogramUpdate (h:hs) 0 = (h + 1) : hs

histogramUpdate (h:hs) n = h : histogramUpdate hs (n - 1)

histogram :: [Int] -> [Int]

histogram = foldl' histogramUpdate []