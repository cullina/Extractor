module Multinomial where

import SubsetSelection
import Uniform

multidecision :: Integral a => [a] -> UnifNat a -> (Int, UnifNat a)
multidecision = md 0
  where 
    md _ [] _     = error "Thresholds too small."
    md n (t:ts) u = 
          case decision t u of
            (True,  u') -> (n, u')
            (False, u') -> md (n + 1) ts u'
            
scaledMultidecision :: Integral a => [a] -> UnifNat a -> (Int, UnifNat a)
scaledMultidecision ts u@(UnifNat b _) = 
  let ts' = map ((b `div` sum ts) *) ts
  in multidecision ts' u
      
decNth :: Integral a => Int -> [a] -> [a]
decNth _ [] = error "Index too large."
decNth 0 (x:xs) = (x - 1) : xs
decNth n (x:xs) = x : decNth (n - 1) xs

multichoose :: Integral a => [a] -> a
multichoose = fst . mc
  where
    mc []     = (1, 0)
    mc (x:xs) =
      let (y, sum) = mc xs
      in (y * choose (x + sum) x, x + sum)