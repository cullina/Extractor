module Partition where

import Multiset
import Data.List(mapAccumR)
import Util(dup, (...), cons, mapFst, toList, churchEncoding, flipNonempty, reverseNonempty, zipWithNonempty)

diffsToFreqs :: (Integral a) => (a, [a]) -> (a, [a])
diffsToFreqs (x, xs) = align x $ foldr dTF (0,[]) xs
  where dTF x = mapFst (1 +) . align x 
        align x = churchEncoding x (cons 0)

diffsToFreqsViaBits = subsetToComposition . map not . compositionToSubset          

partitionToFreqs = diffsToFreqs . partitionToDiffs
  

partitionToDiffs :: (Integral a) => (a, [a]) -> (a, [a])
partitionToDiffs xss@(_,xs) = 
  zipWithNonempty (-) xss (flipNonempty (xs, 1))
  
diffsToPartition :: (Integral a) => (a, [a]) -> (a, [a])
diffsToPartition (x, xs) = mapFst (x +) $ mapAccumR (dup ... (+)) 1 xs

conjugate = diffsToPartition . reverseNonempty . partitionToFreqs

conjugateViaBits = diffsToPartition . reverseNonempty . diffsToFreqsViaBits . partitionToDiffs

