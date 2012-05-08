module Experts where

import Data.List(mapAccumL, foldl')
import Bit
import Histogram

class Expert a where
  predict   :: a -> Double
  update    :: a -> Bool -> a
  

data ConstantExpert = CE Double

instance Expert ConstantExpert where
  predict (CE p) = p
  update e _     = e

simExpert :: (Expert a) => a -> [Bool] -> (a,[(Bool, Double)])
simExpert expert = mapAccumL f expert 
  where 
    f :: (Expert a) => a -> Bool -> (a, (Bool, Double))
    f expert b = (update expert b, (b, predict expert))
    
    
getLoss :: Expert a => (Bool -> Double -> Double) -> a -> [Bool] -> Double
getLoss l e = computeLoss l . snd . simExpert e

computeLoss :: (Bool -> Double -> Double) -> [(Bool, Double)] -> Double
computeLoss f = sum . map (uncurry f)

logLoss :: Bool -> Double -> Double
logLoss True  = log
logLoss False = log . (1 -)

sqLoss :: Bool -> Double -> Double
sqLoss True  x = (x - 1) * (1 - x)
sqLoss False x = (-1) * x * x
 

subseqs :: Int -> [a] -> [[a]]
subseqs n xs = snd $ mapAccumL f ys zs
  where 
    (ys, zs)   = splitAt n xs
    f []     x = ([], [x])
    f (y:ys) x = 
      let zs = ys ++ [x]
      in (zs, y:zs)
         

markov :: Int -> [Bool] -> [Int]
markov n = treeHistogram . map bitsToInt . subseqs n
