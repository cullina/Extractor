module IncreasingSubsequence where

import Permutation
import Data.Function(on)
import Data.List(sort, sortBy)

recoverFullPerm :: (Integral a) => a -> [(a,a)] -> [a]

recoverFullPerm max pairs = take (fromIntegral max) $ rFP 0 sortedPairs outs
  where sortedPairs = sortBy (compare `on` fst) pairs
        outs        = complementInts 0 . sort  . map snd  $ pairs
        
        rFP _ []         zs     = zs
        rFP i pss@((x,y):ps) zss@(z:zs) = 
          if i == x
          then y : rFP (i+1) ps  zss
          else z : rFP (i+1) pss zs
      
complementInts :: (Integral a) => a -> [a] -> [a]
complementInts x []  = [x..]
complementInts x (y:ys) = 
  if x == y
  then complementInts (x + 1) ys
  else x : complementInts (x + 1) (y:ys)
