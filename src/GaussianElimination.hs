module GaussianElimination where

import Uniform(gcdPlus)

rowEchelon :: [[Int]] -> [[Int]]
rowEchelon rs   = search rs []
 where
   search :: [[Int]] -> [[Int]] -> [[Int]]
   search []          zs = zs
   search ([]:ys)     zs = search ys ([] : zs)
   search ((0:xs):ys) zs = search ys (xs : zs)
   search ((x:xs):ys) zs = (x:xs) : rowEchelon (reduce x xs ys zs)
   
   reduce :: Int -> [Int] -> [[Int]] -> [[Int]] -> [[Int]]
   reduce x xs []     zs = reverse zs
   reduce x xs (y:ys) zs = reduce x xs ys ((reduceRow x xs y) : zs)
   
   reduceRow :: Int -> [Int] -> [Int] -> [Int]
   reduceRow x xs [] = []
   reduceRow x xs (y:ys) = 
     let (_, a, b) = gcdPlus x y
     in zipWith (\x y -> a * y - b * x) xs ys
        
