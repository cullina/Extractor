module Partition where

import Multiset
import Data.List(mapAccumR)
import Util(dup, (...), mapHead, churchEncoding)

diffsToFreqs :: (Integral a) => [a] -> [a]
diffsToFreqs = foldr dTF []
  where dTF x = mapHead (1 +) . churchEncoding x (0 :)
          
partitionToFreqs = diffsToFreqs . partitionToDiffs
  

partitionToDiffs :: (Integral a) => [a] -> [a]
partitionToDiffs []         = []
partitionToDiffs xss@(_:xs) = zipWith (-) xss (xs ++ [0])
  
diffsToPartition :: (Integral a) => [a] -> [a]
diffsToPartition = snd . mapAccumR (dup ... (+)) 0

conjugate = diffsToPartition . reverse . partitionToFreqs