module Falling where

import Uniform
import Data.List(unfoldr)


data Falling a = Falling 
                 { maxMaxValue :: a 
                 , fallingValues    :: [a]
                 } deriving Show

data Rising a = Rising 
                { minMaxValue :: a 
                , risingValues    :: [a]
                } deriving Show

splitF :: (Num a) => Falling a -> Maybe (UnifNat a, Falling a)

splitF (Falling m (x:xs)) = Just (UnifNat m x, Falling (m - 1) xs) 
splitF (Falling _ [])     = Nothing

splitR :: (Num a) => Rising a -> Maybe (UnifNat a, Rising a)

splitR (Rising m (x:xs)) = Just (UnifNat m x, Rising (m + 1) xs) 
splitR (Rising _ [])     = Nothing

unifsFromFalling = unfoldr splitF

unifsFromRising = unfoldr splitR

unifToFalling (UnifNat a b) = Falling a [b]

unifToRising (UnifNat a b) = Rising a [b]

safeJoinF (UnifNat um x) (Falling fm xs) = 
  if um == fm + 1
  then Just $ Falling um (x : xs)
  else Nothing

safeJoinR (UnifNat um x) (Rising fm xs) = 
  if um == fm - 1
  then Just $ Rising um (x : xs)
  else Nothing

testUnifs :: (Integral a) => a -> [UnifNat a] -> Bool

testUnifs max = and . zipWith (==) [max, max - 1 ..] . map maxValue


joinF (UnifNat um x) (Falling _ xs) = Falling um (x : xs)


fallingFromUnifs :: (Num a) => [UnifNat a] -> Falling a

fallingFromUnifs = foldr joinF (Falling 0 [])


allFallings max len = sequence . map allUnifs $ take len [max, max - 1 ..]