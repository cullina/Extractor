module SubstringCounting where

import PowerSeries(EOPS(..))
import Matrix
import Data.Monoid

data InvStats = IS Int Int Int
                deriving (Show, Eq, Ord)

instance Monoid InvStats where
  mempty = IS 0 0 0
  mappend (IS a b c) (IS d e f) = IS (a + d) (b + e) (c + f + (b * d))
  
  
bStats :: Bool -> BMat EOPS
bStats False = BMat $ lookupBB (Even [1], Odd [1], Zero, Even [1])
bStats True  = BMat $ lookupBB (Even [1], Zero, Odd [1], Even [1])

bsStats :: [Bool] -> BMat EOPS
bsStats = mconcat . map bStats

diffStats :: BMat EOPS -> EOPS
diffStats (BMat m) = (transpose diffVec `bbMatrixMult` m  `bbMatrixMult` sumVec) ((),())

bInvStats :: Bool -> InvStats
bInvStats False = IS 1 0 0
bInvStats True  = IS 0 1 0

bsInvStats :: [Bool] -> InvStats 
bsInvStats = mconcat . map bInvStats

inversions :: InvStats -> Int
inversions (IS _ _ n) = n
