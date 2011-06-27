module Fibonacci where

import Data.List(foldl')


f = False
t = True

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


fibBitsToNatList = map fibBitsToNat . parseSegments

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


pruneZeroes = dropWhile not
