module Fibonacci where

import Data.List(foldl')
import Bit(pruneZeroes)
import Util(mapSnd, insertNothings, parseNothings)

f = False
t = True

data Fib = Long 
         | Short 
         deriving Show

fBitsToInt :: (Integral a) => [Bool] -> a
fBitsToInt = fst . foldl' fibRecIf (0, 0)

fBitsToNat :: (Integral a) => [Bool] -> a
fBitsToNat = fst . foldl' fibRecIf (1, 1)

fibRecIf (m,n) b = 
    let m' = m + if b then 1 else 0
    in (m' + n, m')

fibs :: (Integral a) => [a]
fibs = map fst $ iterate fibRec (1, 1)
    where fibRec (m, n) = (m + n, m)


intToFBits :: (Integral a) => a -> [Fib]
intToFBits n = toFBits True (reverse (takeWhile (<= n) fibs)) n

intToWFBits :: (Integral a) => Int -> a -> [Fib]
intToWFBits w = toFBits False (reverse (take w fibs))

natToFBits :: (Integral a) => a -> [Fib]
natToFBits = tail . intToFBits

toFBits :: (Integral a) => Bool -> [a] -> a -> [Fib]
toFBits False []     _ = []
toFBits True  []     _ = [Short]
toFBits False (f:fs) n = toFBits True fs n
toFBits True  (f:fs) n =
    if f <= n
    then Long  : toFBits False fs (n - f)
    else Short : toFBits True  fs n


-- F_{n+2} = 1 + sum_{i=0}^n F_i

removeLeadingOne :: [Fib] -> Maybe [Fib]
removeLeadingOne (Short:fs) = removeLeadingOne fs
removeLeadingOne (Long:fs)  = Just fs
removeLeadingOne []         = Nothing

{----}

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
    
streamToFibsSeq :: [Bool] -> [[Fib]]
streamToFibsSeq = parseNothings . snd . parseStream

fibsSeqToStream :: [[Fib]] -> [Bool]
fibsSeqToStream = writeStream False . insertNothings

fBitsToNats :: (Integral a) => [Bool] -> [a]
fBitsToNats = map (fBitsToNat . fibsToFBits) . streamToFibsSeq

natsToFBits :: (Integral a) => [a] -> [Bool]
natsToFBits =  fibsSeqToStream . map natToFBits


fibsToFBits :: [Fib] -> [Bool]
fibsToFBits = concatMap ex
  where ex Long  = [False, True]
        ex Short = [False]

fBitsToFibs :: [Bool] -> Maybe [Fib]
fBitsToFibs [] = Just []
fBitsToFibs (False:(True:bs)) = fmap (Long :) (fBitsToFibs bs)
fBitsToFibs (False:bs) = fmap (Short :) (fBitsToFibs bs)
fBitsToFibs (True : _) = Nothing

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

incrementFib :: [Fib] -> [Fib]
incrementFib = end . foldr merge ([], EQ)
  where merge Short (fs, EQ) = (fs,             LT)
        merge Long  (fs, EQ) = (Short:fs,       LT)
        merge f     (fs, GT) = (f:fs,           GT)
        merge Short (fs, LT) = (Long:fs,        GT)
        merge Long  (fs, LT) = (Short:Short:fs, LT)
        --end merges with implicit Long that prefixes everything
        end (fs, GT) = fs
        end (fs, EQ) = Short:fs
        end (fs, LT) = Short:Short:fs 