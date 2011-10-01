module Util where

import Data.List(intercalate)

cons :: (a, [a]) -> [a]
cons = uncurry (:)

mapFst :: (a -> b) -> (a, c) -> (b, c) 
mapFst f (x, y) = (f x, y)

mapSnd :: (a -> b) -> (c, a) -> (c, b) 
mapSnd f (x, y) = (x, f y)

mapHead :: (a -> a) -> [a] -> [a]
mapHead _ [] = []
mapHead f (x:xs) = f x : xs


(...) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
f ... g = \x y -> f (g x y)

dup :: a -> (a, a)
dup x = (x, x)

maybePred :: (Integral a) => a -> Maybe a
maybePred 0 = Nothing
maybePred n = Just (n - 1)


insertNothings :: [[a]] -> [Maybe a]
insertNothings = intercalate [Nothing] . map (map Just)

parseNothings :: [Maybe a] -> [[a]]
parseNothings = foldr pN [[]] 
  where pN Nothing  xss = [] : xss
        pN (Just x) xss = mapHead (x :) xss
        