module QaryGraphs where

import Graph
import Data.List(foldl', unfoldr)
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
        toQs n = 
          let (n',r) = quotRem n q
          in Just (Qary r,n') 
          
intToLQs :: Int -> Int -> Int -> [Qary]
intToLQs q l n = reverse $ unfoldr toQs (l,n)
  where toQs (0,_) = Nothing
        toQs (l,n) = 
          let (n',r) = quotRem n q
          in Just (Qary r,(l-1,n')) 

qIf :: Int -> Int -> Qary -> Int
qIf q a (Qary b) = q * a + b

qLevelCliques :: [Int] -> CliqueList [Qary]
qLevelCliques = CliqueList . map (qIns . map Qary) . allMultinomials

qLevelIntEdges :: [Int] -> EdgeList Int
qLevelIntEdges ks = organizeEdges . mergeCliques . fmap (qsToInt (length ks)) $ qLevelCliques ks

qMiddleLevelIntEdges :: Int -> Int -> EdgeList Int
qMiddleLevelIntEdges q k = 
  organizeEdges . mergeCliques . fmap (qsToInt q) . qLevelCliques $ replicate q k


qIntEdges :: Int -> Int -> EdgeList Int
qIntEdges q n = organizeEdges . mergeCliques . fmap (qsToInt q) . CliqueList . map (singleQInsertions q) . allQStrings q $ n - 1
                
qInsertionStars :: Int -> Int -> [(Int, [Int])] 
qInsertionStars q n = 
  map (mapFst (qsToInt q) . mapSnd (map (qsToInt q)) . keepArg (singleQInsertions q)) $ allQStrings q (n - 1)

qLevelInsertionStars :: Int -> Int -> [(Int, [Int])]
qLevelInsertionStars q k =
  map (mapFst (qsToInt q) . mapSnd (map (qsToInt q)) . keepArg qIns . map Qary) . allMultinomials $ replicate q k
  
canonize :: [Qary] -> [Qary] -> [Qary]
canonize order = c order []
  where
    c ::  [Qary] -> [(Qary,Qary)] -> [Qary] -> [Qary]
    c _  _  [] = []
    c alphabet fn (i:is) = 
      case translate fn i of
        Just j  -> j : c alphabet fn is
        Nothing -> case alphabet of
          (a:as) -> a : c as ((i,a):fn) is
          []     -> error "empty alphabet"
    
    translate :: [(Qary,Qary)] -> Qary -> Maybe Qary
    translate [] _ = Nothing
    translate ((i,j):ijs) k =
      if i == k
      then Just j
      else translate ijs k

--canonize (map Qary [0..])