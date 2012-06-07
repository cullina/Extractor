module QaryGraphs where

import Graph
import Data.List(foldl', unfoldr)
import Data.Tuple(swap)
import Control.Monad(replicateM)
import LevGraphs(mergeCliques)
import SubsetSelection(allMultinomials)
import Util(keepArg, mapFst, mapSnd)

newtype Qary = Qary Int deriving Eq

instance Show Qary where
  show (Qary x) = show x

allQ :: Int -> [Qary]
allQ q = map Qary [0 .. q - 1]

allQStrings :: Int -> Int -> [[Qary]]
allQStrings q n = replicateM n $ allQ q

qInsertions :: Qary -> [Qary] -> [[Qary]]
qInsertions x []     = [[x]]
qInsertions x (y:ys)
  | x == y    = map (y :) (qInsertions x ys)
  | otherwise = (x:y:ys) : map (y :) (qInsertions x ys)
                
singleQInsertions :: Int -> [Qary] -> [[Qary]]
singleQInsertions q xs = (`qInsertions` xs) =<< allQ q
                
qIns (x:xs) = qInsertions x xs
qIns [] = undefined

qsToInt :: Int -> [Qary] -> Int
qsToInt q = foldl' (qIf q) 0

intToQs :: Int -> Int -> [Qary]
intToQs q = reverse . unfoldr toQs
  where toQs 0 = Nothing
        toQs n = Just . mapFst Qary . swap $ quotRem n q
          
qIf :: Int -> Int -> Qary -> Int
qIf q a (Qary b) = q * a + b

qLevelIntEdges :: [Int] -> EdgeList Int
qLevelIntEdges ks = mergeCliques (qsToInt (length ks)) . map (qIns . map Qary) $ allMultinomials ks

qMiddleLevelIntEdges :: Int -> Int -> EdgeList Int
qMiddleLevelIntEdges q k = 
  mergeCliques (qsToInt q) . map (qIns . map Qary) . allMultinomials $ replicate q k

qIntEdges :: Int -> Int -> EdgeList Int
qIntEdges q n = mergeCliques (qsToInt q) . map (singleQInsertions q) . allQStrings q $ n - 1
                
qInsertionStars :: Int -> Int -> [(Int, [Int])] 
qInsertionStars q n = 
  map (mapFst (qsToInt q) . mapSnd (map (qsToInt q)) . keepArg (singleQInsertions q)) $ allQStrings q (n - 1)

qLevelInsertionStars :: Int -> Int -> [(Int, [Int])]
qLevelInsertionStars q k =
  map (mapFst (qsToInt q) . mapSnd (map (qsToInt q)) . keepArg qIns . map Qary) . allMultinomials $ replicate q k