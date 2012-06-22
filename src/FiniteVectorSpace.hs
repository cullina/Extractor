module FiniteVectorSpace where

hammingDist :: (Eq a) => [a] -> [a] -> Int

hammingDist x y = 
            sum . map (\x -> if x then 0 else 1) $ zipWith (==) x y


sumMod max a b = rem (a + b) max

rotateBy max n = map (sumMod max n)

allEquivalent max x = 
    map (\n -> rotateBy max n x) [0..max-1]

minHammingDist max x y = 
    minimum $ map (hammingDist x) (allEquivalent max y)


maxDist max length x y = 
    minHammingDist max x y == length - 1


allReducedVectors max length = 
     map (0:) $ allVectors max (length - 1)

allVectors _ 0 = [[]]

allVectors max length = 
    let shorterVectors = allVectors max (length - 1)
    in concatMap (\x -> map (x:) shorterVectors) [0..max-1]

greedyMaximalSet f = greedyMaximalSet' f []

greedyMaximalSet' _ set [] = set

greedyMaximalSet' f xs (y:ys) = greedyMaximalSet' f (y:xs) (filter (f y) ys)


maximumN p k = (+) k . length $ maximalSet p k

maximalSet p k = greedyMaximalSet (maxDist (p-1) k) $ allReducedVectors (p-1) k