module Bitstream where

import System.Random

--Utility


bitsToInt = foldl doubleIf 0

doubleIf a b = if b
               then 2*a + 1
               else 2*a


intToBits 0 bits = bits
intToBits n bits = let (q,r) = quotRem n 2
                   in intToBits q ((r == 1) : bits)


intWToBits 0 n = []

intWToBits w n = let (q,r) = quotRem n 2
                 in (r == 1) : intWToBits (w - 1) q


maxInBits n = intToBits (n - 1) []


--bitstreams
{------}{------}{------}{------}{------}{------}{------}{------}{------}{------}

getBit ([], n) = error ("Cannot getBit.  Bitstream is empty after " ++ 
                        show n ++ " bits were used from it.")

getBit ((b:bs), n) = (b, (bs, n+1))

bitStream w = attachCounter . (concatMap (intWToBits w)) . intStream . mkStdGen

attachCounter x = (x, 0)


intStream :: RandomGen r => r -> [Int]

intStream gen = let (int, newGen) = random gen
                in int : intStream newGen

generateList f bs = let (x, bs') = f bs

                    in x : generateList f bs'

generateFiniteList f n bs = generateFiniteList' f n ([], bs)

generateFiniteList' f 0 p = p

generateFiniteList' f n (xs, bs) = let (x, bs') = f bs
                                   in generateFiniteList' f (n - 1) (x:xs, bs')


--Unbias

vNUnbias bs = let (b1, bs') = getBit bs
                  (b2, bs'') = getBit bs'
              in if b1 == b2
                 then vNUnbias bs''
                 else (b2, bs'')


vNStream :: Int -> ([Bool], Int)

vNStream = attachCounter . (generateList vNUnbias) . (bitStream 64)
