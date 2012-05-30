module SubstringCounting where

import PowerSeries(seriesMult, seriesAdd, seriesOne, seriesZero)
import Data.Monoid
import Data.Tuple(swap)

data SubStats = SS ((Bool,Bool) -> [Int])

instance Show SubStats where
  show = show . smallStats 4

instance Monoid SubStats where
  mempty = SS $ lookupBB (seriesOne, seriesZero, seriesZero, seriesOne)
  mappend (SS x) (SS y) = SS $ evenOddMult x y

data InvStats = IS Int Int Int
                deriving (Show, Eq, Ord)

instance Monoid InvStats where
  mempty = IS 0 0 0
  mappend (IS a b c) (IS d e f) = IS (a + d) (b + e) (c + f + (b * d))
  
  
bStats :: Bool -> SubStats
bStats False = SS $ lookupBB (seriesOne, seriesOne, seriesZero, seriesOne)
bStats True  = SS $ lookupBB (seriesOne, seriesZero, seriesOne, seriesOne)

bsStats :: [Bool] -> SubStats
bsStats = mconcat . map bStats

smallStats :: Int -> SubStats -> ([Int],[Int],[Int],[Int])
smallStats n (SS x) = expandBB $ take n . x

bInvStats :: Bool -> InvStats
bInvStats False = IS 1 0 0
bInvStats True  = IS 0 1 0

bsInvStats :: [Bool] -> InvStats 
bsInvStats = mconcat . map bInvStats

inversions :: InvStats -> Int
inversions (IS _ _ n) = n

lookupBB :: (a, a, a, a) -> (Bool,Bool) -> a
lookupBB (a,_,_,_) (False, False) = a
lookupBB (_,b,_,_) (False, True)  = b
lookupBB (_,_,c,_) (True,  False) = c
lookupBB (_,_,_,d) (True,  True)  = d

expandBB :: ((Bool, Bool) -> a) -> (a,a,a,a)
expandBB f = (f (False,False), f (False,True), f (True,False), f (True,True))


evenOddEntryMult :: ((Bool,Bool) -> [Int]) -> ((Bool,Bool) -> [Int]) -> Bool -> Bool -> Bool -> [Int]
evenOddEntryMult a b False False True  = 0 : entryMult a b False False True
evenOddEntryMult a b True  True  False = 0 : entryMult a b True  True  False
evenOddEntryMult a b i j k             = entryMult a b i j k

entryMult :: ((Bool,Bool) -> [Int]) -> ((Bool,Bool) -> [Int]) -> Bool -> Bool -> Bool -> [Int]
entryMult a b i j k = seriesMult (a (i, k)) (b (k, j)) 

-- Multiplies matrices of this form
-- ( A(z^2)   zB(z^2) ) 
-- (zC(z^2)    D(z^2) ) 
evenOddMult :: ((Bool,Bool) -> [Int]) -> ((Bool,Bool) -> [Int]) -> ((Bool,Bool) -> [Int])
evenOddMult a b (i, j) = seriesAdd (f False) (f True)
  where f = evenOddEntryMult a b i j