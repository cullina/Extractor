module LevOps where

import Bit(bitsToInt, allBitStrings, xor, alt)
import SubsetSelection(allSubsets, getSubset)
import Util(church)
import ListSet(listSetFromList, fastHist)
import SubstringCounting
import Data.List(mapAccumL)
import Data.Maybe(mapMaybe)
import Control.Monad((<=<))
import Control.Applicative((<$>))
import Data.List(foldl')

allSingleInsertions :: [Bool] -> [[Bool]]
allSingleInsertions []     = [[True],[False]]
allSingleInsertions (b:bs) = (not b : b : bs) : map (b :) (allSingleInsertions bs)

allSingleZeroInsertions :: [Bool] -> [[Bool]]
allSingleZeroInsertions []         = [[False]]
allSingleZeroInsertions (True:bs)  = (False : True : bs) : map (True :) (allSingleZeroInsertions bs)
allSingleZeroInsertions (False:bs) = map (False :) (allSingleZeroInsertions bs)

allSingleOneInsertions :: [Bool] -> [[Bool]]
allSingleOneInsertions []         = [[True]]
allSingleOneInsertions (True:bs)  = map (True :) (allSingleOneInsertions bs)
allSingleOneInsertions (False:bs) = (True : False : bs) : map (False :) (allSingleOneInsertions bs)


atMostSOnes n s = allSubsets n =<< [0..s]

-- second arg should have at most s ones
insertion :: [Bool] -> [Bool] -> [Bool]
insertion [] ps = ps
insertion (b:bs) (True:ps)  = not b : insertion (b:bs) ps
insertion (b:bs) (False:ps) = b : insertion bs ps
insertion _ [] = []


deletion :: [Bool] -> [Bool] -> Maybe [Bool]
deletion _      []         = Just []
deletion []     (_:_)      = Nothing
deletion (b:bs) (False:ps) = (b :) <$> deletion bs ps
deletion (b:bs) (True:ps)  = (not b :) <$> (flip deletion ps =<< match (not b) bs)
    
match :: Bool -> [Bool] -> Maybe [Bool]
match _ []     = Nothing 
match m (b:bs)
  | m == b = Just bs
  | otherwise = match m bs


insertion2 :: [Bool] -> [Bool] -> [Bool]
insertion2 bs = snd . mapAccumL f bs
  where 
    f []     p     = ([],   p)
    f (b:bs) True  = (b:bs, not b)
    f (b:bs) False = (bs,   b)

deletion2 :: [Bool] -> [Bool] -> Maybe [Bool]
deletion2 bs =  test . mapAccumL f (Just bs)
  where 
    f Nothing       p     = (Nothing, p)
    f (Just [])     p     = (Nothing, p)
    f (Just (b:bs)) True  = (match (not b) bs, not b)
    f (Just (b:bs)) False = (Just bs, b)
    
    test (Just _, xs) = Just xs
    test (Nothing, _) = Nothing



allInsertions :: Int -> [Bool] -> [[Bool]]
allInsertions s bs = map (insertion2 bs) (atMostSOnes (length bs + s) s)

allDeletions :: Int -> [Bool] -> [[Bool]]
allDeletions s bs = mapMaybe (deletion2 bs) (atMostSOnes (length bs - s) s)


allDeletions2 :: Ord a => Int -> [a] ->[[a]]
allDeletions2 s bs = listSetFromList . map (getSubset bs . map not) . allSubsets (length bs) $ s

dSize s = length . allDeletions s

-------------------------
hWeight = length . filter id

vtSum = sum . getSubset [1..]

vtWeight bs = vtWeightM (length bs + 1) bs

vtWeightM m bs = vtSum bs `mod` m

vthClass n w = let subs = allSubsets n w ++ allSubsets n (w+1)
               in \k -> filter ((k ==) . vtWeight) subs

vthClasses n w = map (vthClass n w) [0..n]

vtClass n k = filter ((k ==) . vtWeight) (allBitStrings n)

vtClasses n = map (vtClass n) [0..n]

vtLevelClass n k a = filter ((a ==) . vtWeightM (1 + max k (n - k))) (allSubsets n k) 

vtLevelClassSizes n k =   
  let k' = max k (n-k) 
  in fastHist . map (vtWeightM (1 + k')) $ allSubsets n k

bigVTLevelClass n k = maximum . map snd $ vtLevelClassSizes n k

bigVTLevelClasses n = map (bigVTLevelClass n) [0..n]

genWeight ws max = (`mod` max) . sum . getSubset ws 


neighborhood :: Int -> [Bool] -> Int
neighborhood s = length . listSetFromList . map bitsToInt . (allInsertions s <=< allDeletions s)

data P = P !Int !Int deriving Show

maxDegree :: Int -> Int -> P
maxDegree s n = 
  let 
    xs      = atMostSOnes n s
    ys      = atMostSOnes (n - s) s
    allD bs = mapMaybe (deletion2 bs) ys
    allI bs = map (insertion2 bs) xs
    maxSum (P a b) n = P (max a n) (b + n - 1)
    neigh = length . listSetFromList . map bitsToInt . (allI <=< allD)
  in foldl' maxSum (P 0 0) . map neigh $ allBitStrings n


----

fromAntiruns :: [Int] -> [Bool]
fromAntiruns = zipWith xor (alt False) . fromRuns

fromRuns :: [Int] -> [Bool]
fromRuns ns = concatMap (uncurry replicate) $ zip ns (alt False)

        
evenAntiruns k m = fromAntiruns $ replicate k m

----

allMirror :: Int -> [[Bool]]
allMirror = map f . allBitStrings
  where f bs = bs ++ reverse (map not bs)

----------

derivative :: [Bool] -> [Bool]
derivative [] = []
derivative [_] = []
derivative (x:y:zs) = xor x y : derivative (y:zs)

hn :: Int -> [Bool] -> Int
hn n = hWeight . church n derivative

-----------

inversionStats :: Int -> [Bool] -> [(InvStats, Int)]
inversionStats s = fastHist . map bsInvStats . allInsertions s

allEqual [] = True
allEqual (x:xs) = all (x ==) xs