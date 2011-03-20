module Bit
    (
     bitsToInt,
     bitsToNat,
     doubleIf,
     intToBits,
     natToBits,
     intWToBits,
     maxInBits,
     xor,
     incrementInt,
     incrementNat
    )
where

import Data.List(foldl')


--Utility

-- most significant bits first

bitsToInt = foldl' doubleIf 0

bitsToNat = foldl' doubleIf 1

doubleIf a b = 2 * a + if b then 1 else 0


intToBits bits 0 = bits
intToBits bits n = 
    let (q, r) = quotRem n 2
    in intToBits ((r == 1) : bits) q


natToBits bits 1 = bits
natToBits bits n = 
    let (q, r) = quotRem n 2
    in natToBits ((r == 1) : bits) q


intWToBits :: (Integral a, Integral b) => a -> [Bool] -> b -> [Bool]

intWToBits 0 bits n = bits

intWToBits w bits n = let (q, r) = quotRem n 2
                      in intWToBits (w - 1) ((r == 1) : bits) q


maxInBits n = intToBits [] (n - 1)


xor True = not
xor False = id

incrementInt bs = 
    let (lowBits, carry) = incrementInt' bs
    in if carry
       then True : lowBits
       else lowBits

incrementNat bs = 
    let (lowBits, carry) = incrementInt' bs
    in if carry
       then False : lowBits
       else lowBits

incrementFixedWidthInt bs = fst $ incrementInt' bs

incrementInt' [] = ([], True)

incrementInt' (b:bs) = 
    let (lowBits, carry) = incrementInt' bs
    in (xor b carry : bs, (&&) b carry)  
