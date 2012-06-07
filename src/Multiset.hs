module Multiset where

import SubsetSelection
import Data.List(unfoldr)
-- multisets/compositions

subsetToComposition = foldr sTC (0, [])
  where sTC True  (n, ns) = (0, n:ns)
        sTC False (n, ns) = (n+1, ns)
                      
compositionToSubset = unfoldr cTS
  where cTS (0, [])   = Nothing
        cTS (0, n:ns) = Just (True,  (n,   ns))
        cTS (n, ns)   = Just (False, (n-1, ns))

subsetToSpanningComposition = foldr sTSC (1, [])
  where sTSC True  (n, ns) = (1, n:ns)
        sTSC False (n, ns) = (n+1, ns)
                      
spanningCompositionToSubset = unfoldr sCTS
  where sCTS (1, [])   = Nothing
        sCTS (1, n:ns) = Just (True,  (n,   ns))
        sCTS (n, ns)   = Just (False, (n-1, ns))

equalityToInequality (x, xs) = (xs, x + sum xs)

inequalityToEquality (xs, bound) = (bound - sum xs, xs)