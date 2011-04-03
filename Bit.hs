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
     incrementNat,
     toPrefixCode,
     fromPrefixCode
    )
where

import Data.List(foldl')

f = False
t = True

--Utility

-- most significant bits first

bitsToInt = foldl' doubleIf 0

bitsToNat = foldl' doubleIf 1


doubleIf a b = 
    2 * a + if b then 1 else 0

intToBits :: (Integral a) => a -> [Bool]

intToBits = toBits []

natToBits :: (Integral a) => a -> [Bool]

natToBits = tail . toBits []


toBits bits 0 = bits
toBits bits n = 
    let (q, r) = quotRem n 2
    in toBits ((r == 1) : bits) q

                 

intWToBits :: (Integral a, Integral b) => a -> [Bool] -> b -> [Bool]

intWToBits 0 bits n = bits

intWToBits w bits n = let (q, r) = quotRem n 2
                      in intWToBits (w - 1) ((r == 1) : bits) q


maxInBits n = intToBits (n - 1)


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
    in (xor b carry : lowBits, (&&) b carry)  





toPrefixCode max n = 
    let bits = intWToBits (length max) [] n
    in toPrefixCode' (zip max bits)

toPrefixCode' [] = []

toPrefixCode' ((True,True):bs) = True : toPrefixCode' bs

toPrefixCode' ((True,False):bs) = False : map snd bs

toPrefixCode' ((False, _):bs) = toPrefixCode' bs


fromPrefixCode [] bs = Just ([], bs)

fromPrefixCode (False:max) bs = False `consFst` fromPrefixCode max bs

fromPrefixCode (True:max) [] = Nothing

fromPrefixCode (True:max) (True:bs) = True `consFst` fromPrefixCode max bs

fromPrefixCode (True:max) (False:bs) = False `consFst` safeSplitAt max bs


safeSplitAt [] bs = Just ([], bs)

safeSplitAt (m:ms) [] = Nothing

safeSplitAt (m:ms) (b:bs) = b `consFst` safeSplitAt ms bs



consFst x (Just (xs,y)) = Just (x:xs, y)

consFst x Nothing = Nothing