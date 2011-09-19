module Fibonacci where

import Data.List(foldl')
import Bit(pruneZeroes)
import Util(mapSnd)

f = False
t = True

data Fib = Long 
         | Short 
         deriving Show

fibBitsToInt = fst . foldl' fibRecIf (0, 0)

fibBitsToNat = fst . foldl' fibRecIf (1, 1)

fibRecIf (m,n) b = 
    let m' = m + if b then 1 else 0
    in (m' + n, m')


fibs = map fst $ iterate fibRec (1, 1)
    where fibRec (m, n) = (m + n, m)



intToFibBits n = toFibBits (reverse (takeWhile (<= n) fibs)) n

natToFibBits n = tail $ toFibBits (reverse (takeWhile (<= n) fibs)) n

toFibBits [] _ = []

toFibBits (f:fs) n =
    if f <= n
    then True  : toFibBits fs (n - f)
    else False : toFibBits fs n

{----}

consFst x (xs,y) = (x:xs, y)


parseSegments (True:bs) = [] : parseSegments bs

parseSegments bs = 
    let (x,y) = splitSegs bs
    in case y of
         Nothing -> x :[]
         Just yy -> x : parseSegments yy

splitSegs [] = ([], Nothing)
splitSegs (True:(True:bs)) = ([], Just bs)
splitSegs (b:bs) = b `consFst` splitSegs bs


parseStream :: [Bool] -> (Bool, [Maybe Fib])
parseStream = fromClean
  where fromClean []         = (False, [])
        fromClean (True:bs)  = fromOne bs
        fromClean (False:bs) = mapSnd (Just Short :) (fromClean bs)
        
        fromOne []           = (True, [])
        fromOne (True:bs)    = mapSnd (Nothing :) (fromClean bs)
        fromOne (False:bs)   = mapSnd (Just Long :) (fromClean bs)

writeStream :: Bool -> [Maybe Fib] -> [Bool]
writeStream b fs = concatMap wS fs ++ seed b
  where seed True       = [True]
        seed False      = []
        wS Nothing      = [True, True]
        wS (Just Short) = [False]
        wS (Just Long)  = [True, False]
    

fibBitsToNatList = map fibBitsToNat . parseSegments

expand = concatMap ex
  where ex Long  = [False, True]
        ex Short = [False]

translate [] = []

translate (False:(True:bs)) = True : translate bs

translate (False:bs) = False : translate bs

translate (True : _) = error "Initial or consecutive Trues." 

{----}


normalizeFib bs =
    let bs'            = removeLongRuns False False False bs
        (b1, b2, bs'') = removePairs False False bs'
    in pruneZeroes $ b1 : b2 : bs''


removeLongRuns False True True bs = 
    removeLongRuns True False False bs
    
removeLongRuns b1 b2 b3 [] = [b1, b2, b3]

removeLongRuns b1 b2 b3 (b4:bs) = b1 : removeLongRuns b2 b3 b4 bs


--correctness depends on b1 b2 b3 not all being true
--
removePairs b1 b2 [] = (b1, b2, [])

removePairs b1 b2 (b3:bs) =
    case removePairs b2 b3 bs of
      (True, True, bs') -> (True, False, False : bs')
      (b2',  b3',  bs') -> (b1,   b2',   b3'   : bs')


incrementFib' True  True  _  = error "Consecutive Trues."
incrementFib' b1    False [] = (b1, True, [])
incrementFib' False True  [] = (True, False, [])

incrementFib' b1 b2 (b3:bs) =
    case incrementFib' b2 b3 bs of
      (True, True, bs') -> (True, False, False : bs')
      (b2',  b3',  bs') -> (b1,   b2',   b3'   : bs')

incrementFib bs =
    let (b1, b2, bs') = incrementFib' False False bs
    in pruneZeroes $ b1 : b2 : bs'


incrementFib2 = end . foldr merge ([], EQ)
  where --inc []      = ([Short], False)
        --inc [Short] = ([], True)
        --inc [Long]  = ([Short], True)
        --inc (f:fs) = merge f (inc fs)
        
        merge Short (fs, EQ) = (fs,             LT)
        merge Long  (fs, EQ) = (Short:fs,       LT)
        merge f     (fs, GT) = (f:fs,           GT)
        merge Short (fs, LT) = (Long:fs,        GT)
        merge Long  (fs, LT) = (Short:Short:fs, LT)
        --end merges with implicit Long that prefixes everything
        end (fs, GT) = fs
        end (fs, EQ) = Short:fs
        end (fs, LT) = Short:Short:fs 