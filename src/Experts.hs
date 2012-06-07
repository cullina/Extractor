module Experts where

import Data.List(mapAccumL, foldl', tails)
import Bit
import Histogram

class Expert a where
  predict     :: a -> Double
  updateState :: Bool -> a -> a
  updateModel :: Bool -> a -> a
  update      :: Bool -> a -> a
  update newB = updateState newB . updateModel newB
  

data ConstantExpert = CE Double

instance Expert ConstantExpert where
  predict (CE p)  = p
  updateState _ e = e
  updateModel _ e = e

simExpert :: (Expert a) => a -> [Bool] -> (a,[(Bool, Double)])
simExpert = mapAccumL f
  where 
    f :: (Expert a) => a -> Bool -> (a, (Bool, Double))
    f expert b = (update b expert, (b, predict expert))
    
simTwoExpert :: (Expert a) => (a,a) -> [Bool] -> ((a,a),[(Bool, Double)])
simTwoExpert = mapAccumL f
  where 
    f :: (Expert a) => (a,a) -> Bool -> ((a,a), (Bool, Double))
    f (e1, e2) b = ((update b e1, update b e2), (b, (predict e1 + predict e2) / 2))



getLoss :: Expert a => (Bool -> Double -> Double) -> a -> [Bool] -> Double
getLoss l e = computeLoss l . snd . simExpert e

computeLoss :: (Bool -> Double -> Double) -> [(Bool, Double)] -> Double
computeLoss f = sum . map (uncurry f)

----------------------------------

mapNth :: Int -> (a -> a) -> [a] -> [a]
mapNth _ _ [] = []
mapNth 0 f (x:xs) = f x : xs
mapNth n f (x:xs) = x : mapNth (n-1) f xs

simSideExpert :: (Expert a) => Int -> a -> [(Int, Bool)] -> ([a],[(Bool,Double)])
simSideExpert n expert = mapAccumL f (replicate n expert)
  where
    f :: (Expert a) => [a] -> (Int,Bool) -> ([a], (Bool, Double))
    f experts (side, b) = 
      (map (updateState b) . mapNth side (updateModel b) $ experts, (b, predict (experts !! side)))

sideInfo :: Int -> [Bool] -> [Bool] -> [(Int, Bool)]
sideInfo n xs ys = zip (map (bitsToInt . take (n + 1)) (tails xs)) (drop n ys)

getSideLoss ::  Expert a => (Bool -> Double -> Double) -> Int -> a -> [Bool] -> [Bool] -> Double
getSideLoss l n e side = computeLoss l . snd . simSideExpert (2 ^ (n + 1)) e . sideInfo n side

----------------------------------

logLoss :: Bool -> Double -> Double
logLoss True  = log
logLoss False = log . (1 -)

sqLoss :: Bool -> Double -> Double
sqLoss True  x = (x - 1) * (1 - x)
sqLoss False x = (-1) * x * x
 
----------------------------------






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
