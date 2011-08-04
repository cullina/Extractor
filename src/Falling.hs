module Falling where

import Uniform
import Data.List(unfoldr)


data Falling a = Falling 
                 { firstMaxValue :: a 
                 , unifValues    :: [a]
                 } deriving Show

splitF :: (Num a) => Falling a -> Maybe (UnifNat a, Falling a)

splitF (Falling m (x:xs)) = Just (UnifNat m x, Falling (m - 1) xs) 
splitF (Falling _ [])     = Nothing

unifsFromFalling = unfoldr splitF

testUnifs :: (Integral a) => a -> [UnifNat a] -> Bool

testUnifs max = and . zipWith (==) [max, max - 1 ..] . map maxValue


joinF (UnifNat um x) (Falling _ xs) = Falling um (x : xs)


fallingFromUnifs :: (Num a) => [UnifNat a] -> Falling a

fallingFromUnifs = foldr joinF (Falling 0 [])


slowPerm = sP . unifValues  
  where sP []     = []
        sP (u:us) = u : map (\x -> if x >= u then x + 1 else x) (sP us)
  
slowUnperm m ps = Falling m $ slowUnperm' m ps where
  slowUnperm' _ []     = []
  slowUnperm' m (p:ps) = 
    p : map (\x -> if x > p then x - 1 else x) (slowUnperm' (m - 1) ps)
    

