module Bitstream 
    (
     RState(..),
     Bitstream(..),
     bitsToInt,
     intToBits,
     intWToBits,
     maxInBits,
     getBit,
     mapBitstream
    )
where

import System.Random
import Data.List(foldl', unfoldr)
import Control.Monad.State

--Utility


bitsToInt = foldl doubleIf 0

doubleIf a b = 2 * a + if b then 1 else 0


intToBits bits 0 = bits
intToBits bits n = let (q, r) = quotRem n 2
                   in intToBits ((r == 1) : bits) q


intWToBits :: (Integral a, Integral b) => a -> [Bool] -> b -> [Bool]

intWToBits 0 bits n = bits

intWToBits w bits n = let (q, r) = quotRem n 2
                      in intWToBits (w - 1) ((r == 1) : bits) q


maxInBits n = intToBits [] (n - 1)


--bitstreams
{------}{------}{------}{------}{------}{------}{------}{------}{------}{------}

type RState a = State Bitstream a

data Bitstream = Bitstream [Bool] Int

newBitstream x = Bitstream x 0


getBit (Bitstream [] n) = error ("Cannot getBit.  Bitstream is empty after " ++ 
                        show n ++ " bits were used from it.")

getBit (Bitstream (b:bs) n) = (b, Bitstream bs (n + 1))

getBitM = State getBit


bitstreamFromInts :: Int -> [Int] -> Bitstream

bitstreamFromInts w = 
    newBitstream . concatMap (intWToBits w [])

stdBitstream = 
    newBitstream . randomStream

randomStream = unfoldr (Just . random) . mkStdGen

{--------}

generateList :: RState a -> Bitstream -> [a]

generateList m = unfoldr (Just . runState m)




generateFiniteList f n bs = 
    generateFiniteList' f n ([], bs)

generateFiniteList' f 0 p = p

generateFiniteList' f n (xs, bs) = 
    let (x, bs') = f bs
    in generateFiniteList' f (n - 1) (x:xs, bs')
   

 
generateFiniteListM :: RState a -> Int -> RState [a]

generateFiniteListM m n =
    generateFiniteListM' m n (return []) 

generateFiniteListM' :: RState a ->  Int -> RState [a] -> RState [a]

generateFiniteListM' m 0 mm = mm

generateFiniteListM' m n mm = 
    generateFiniteListM' m (n - 1) (liftM2 (:) m mm)

{-
generateFiniteListM' m n mm = 
    generateFiniteListM' m (n - 1) (mm >>= append m)


append :: RState a -> [a] -> RState [a]

append m xs = liftM (: xs) m
-}

mapBitstream :: (a -> b -> (c, b)) -> [a] -> b -> ([c], b)

mapBitstream f as bs = foldl' (foldHelper f) ([], bs) as


foldHelper :: (a -> b -> (c, b)) -> ([c], b) -> a -> ([c], b)

foldHelper f (cs, b) a = 
    let (c, newB) = f a b
    in (c:cs, newB)




--Unbias

vNUnbias bs = let (b1, bs') = getBit bs
                  (b2, bs'') = getBit bs'
              in if b1 == b2
                 then vNUnbias bs''
                 else (b2, bs'')

vNUnbiasM = State vNUnbias


vNStream = newBitstream . (generateList vNUnbiasM)
