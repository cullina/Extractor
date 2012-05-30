module PowerSeries where

import Data.List(unfoldr)
import Util((...))

reverseInits :: [a] -> [[a]]
reverseInits xs = unfoldr f ([], xs)
  where
    f :: ([a], [a]) -> Maybe ([a], ([a],[a]))
    f (ys, [])   = Nothing
    f (ys, x:xs) = 
      let ys' = x:ys
      in Just (ys', (ys',xs))

convolve :: [a] -> [b] -> [[(a,b)]]
convolve xs = map (zip xs) . reverseInits

seriesMult :: (Num a) => [a] -> [a] -> [a]
seriesMult = map (sum . map (uncurry (*))) ... convolve

seriesAdd  :: (Num a) => [a] -> [a] -> [a]
seriesAdd = zipWith (+)

seriesOne :: [Int]
seriesOne = 1 : repeat 0

seriesZero :: [Int]
seriesZero = repeat 0