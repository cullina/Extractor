module Util where

import Data.List(intercalate)

mapFst f (x, y) = (f x, y)
mapSnd f (x, y) = (x, f y)

mapHead _ [] = []
mapHead f (x:xs) = f x : xs

f ... g = \x y -> f (g x y)

dup x = (x, x)

maybePred 0 = Nothing
maybePred n = Just (n - 1)


mapFold :: (b -> a -> (a,b)) -> b -> [a] -> ([a],b)
mapFold _ b []     = ([], b)
mapFold f y (x:xs) = 
  let (xx, yy) = f y x  
  in mapFst (xx :) (mapFold f yy xs) 

insertNothings :: [[a]] -> [Maybe a]
insertNothings = intercalate [Nothing] . map (map Just)

parseNothings :: [Maybe a] -> [[a]]
parseNothings = foldr pN [[]] 
  where pN Nothing  xss = [] : xss
        pN (Just x) xss = mapHead (x :) xss
        