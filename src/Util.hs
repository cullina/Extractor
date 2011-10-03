module Util where

import Data.List(intercalate)

toList :: (a, [a]) -> [a]
toList = uncurry (:)

toNonemptyList :: [a] -> Maybe (a, [a])
toNonemptyList []     = Nothing
toNonemptyList (x:xs) = Just (x, xs)

cons :: a -> (a, [a]) -> (a, [a])
cons x xs = (x, toList xs)

flipNonempty :: ([a], a) -> (a, [a])
flipNonempty ([],   y) = (y, [])
flipNonempty (x:xs, y) = (x, xs ++ [y])

reverseNonempty :: (a, [a]) -> (a, [a])
reverseNonempty (x, xs) = flipNonempty (reverse xs, x)

mapNonempty f (x, xs) = (f x, map f xs)

zipNonempty (x, xs) (y, ys) = ((x, y), zip xs ys)

zipWithNonempty f (x, xs) (y, ys) = (f x y, zipWith f xs ys)

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

churchEncoding :: (Integral a) => a -> (b -> b) -> b -> b
churchEncoding 0 _ = id
churchEncoding n f = f . churchEncoding (n - 1) f




insertNothings :: [[a]] -> [Maybe a]
insertNothings = intercalate [Nothing] . map (map Just)

parseNothings :: [Maybe a] -> [[a]]
parseNothings = foldr pN [[]] 
  where pN Nothing  xss = [] : xss
        pN (Just x) xss = mapHead (x :) xss
        