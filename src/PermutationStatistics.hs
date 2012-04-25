module PermutationStatistics where

import Tree
import Permutation(allPermutations, inverse)

ignoresInverse :: (Eq a) => ([Int] -> a) -> Int -> Bool
ignoresInverse f  = all g . allPermutations
  where
    g p = f p == f (inverse p)
    
descents []     = []
descents (p:ps) = zipWith (<) ps (p:ps)
    
des :: [Int] -> Int
des = length . filter id . descents
