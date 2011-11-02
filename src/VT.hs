module VT where

import Data.Set(Set, fromList, unions, size)

vt = [1,1,2,2,4,6,10,16,30,52,94,172,316,586,1096,
      2048,3856,7286,13798,26216,49940,95326,182362,
      349536,671092,1290556,2485534,4793492,9256396,
      17895736,34636834,67108864,130150588,252645136,
      490853416]
     
allInsertions :: [Bool] -> [[Bool]]
allInsertions []     = [[True],[False]]
allInsertions (b:bs) = (not b : b : bs) : map (b :) (allInsertions bs)

cover :: [[Bool]] -> Set [Bool]
cover = unions . map (fromList . allInsertions)

t = True
f = False