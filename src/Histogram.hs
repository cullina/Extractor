module Histogram 
       ( 
         histogram,
         treeHistogram,
         flatten,
         mean
       ) where

import Data.List(foldl')
import Data.Maybe(fromMaybe)
import BinaryTree



--histogram

histogramUpdate [] 0 = [1]

histogramUpdate [] n = 0 : histogramUpdate [] (n - 1)

histogramUpdate (h:hs) 0 = (h + 1) : hs

histogramUpdate (h:hs) n = h : histogramUpdate hs (n - 1)


histogram :: [Int] -> [Int]

histogram = foldl' histogramUpdate []

{----------------------------------------------------------------}

incNode Nothing = Just 1

incNode (Just n) = Just (n+1)

treeHistogramUpdate tree = modifyNode incNode tree . natToBits . (+) 1 

treeHistogram :: (Integral a) => [a] -> [a]
treeHistogram = map (fromMaybe 0) . flatten . foldl' treeHistogramUpdate Leaf

----------------

mean xs = fromIntegral (sum (zipWith (*) [0..] xs)) / fromIntegral (sum xs)
