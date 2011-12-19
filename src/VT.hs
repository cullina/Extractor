module VT where

import Bit(bitsToInt, allBitStrings)
import SubsetSelection(allSubsets, allSubsetsOf, getSubset)
import Data.Set(Set, fromList, unions, isSubsetOf, member, elems, size, intersection)
import Data.List(sort, sortBy, mapAccumL, delete)
--import Data.Graph.Inductive.Graph
import Util(keepArg2, andTest, mapPair, argMaximumsSoFar)
import Data.Function(on)
import Levenshtein(levDist)

vt = [1,1,2,2,4,6,10,16,30,52,94,172,316,586,1096,
      2048,3856,7286,13798,26216,49940,95326,182362,
      349536,671092,1290556,2485534,4793492,9256396,
      17895736,34636834,67108864,130150588,252645136,
      490853416]
     

cover :: [[Bool]] -> Set [Bool]
cover = unions . map (fromList . allSingleInsertions)

t = True
f = False




metricGraph :: (Ord b) => (a -> a -> b) -> b -> [a] -> [(a,a)]
metricGraph metric radius =
  filter ((radius >=) . uncurry metric) . allPairs


partialComplement vs es = asymDiff (sort . allPairs . sort $ vs) (sort es)
  where asymDiff [] _          = []
        asymDiff xs []         = xs
        asymDiff (x:xs) (y:ys) = case compare x y of
          LT -> x : asymDiff xs ys
          EQ -> asymDiff xs ys
          GT -> asymDiff (x:xs) ys
                      
testColoring f = filter (uncurry (==) . mapPair f)

--------------
