module QaryGraphs where

import Graph
import Data.List(foldl', unfoldr)
import Data.Tuple(swap)
import LevGraphs(mergeCliques)
import SubsetSelection(allMultinomials)

allQ :: Int -> [Int]
allQ q = [0 .. q - 1]

allQStrings :: Int -> Int -> [[Int]]
allQStrings q n = sequence . replicate n $ allQ q

qInsertions :: Int -> [Int] -> [[Int]]
qInsertions x []     = [[x]]
qInsertions x (y:ys)
  | x == y    = map (y :) (qInsertions x ys)
  | otherwise = (x:y:ys) : map (y :) (qInsertions x ys)
                
singleQInsertions :: Int -> [Int] -> [[Int]]
singleQInsertions q xs = (flip qInsertions xs) =<< allQ q
                
qIns (x:xs) = qInsertions x xs
                
qsToInt :: Int -> [Int] -> Int
qsToInt q = foldl' (qIf q) 0

intToQs :: Int -> Int -> [Int]
intToQs q = reverse . unfoldr toQs
  where toQs 0 = Nothing
        toQs n = Just . swap $ quotRem n q
          

qIf q a b = q * a + b

qLevelIntEdges :: [Int] -> EdgeList Int
qLevelIntEdges ks = mergeCliques (qsToInt (length ks)) . map qIns $ allMultinomials ks

qIntEdges :: Int -> Int -> EdgeList Int
qIntEdges q n = mergeCliques (qsToInt q) . map (singleQInsertions q) $ allQStrings q (n - 1)