module PrimitivePoly (getCharPoly) where

import Prime
import BinaryField
import SubsetSelection


-- Check that x generates the whole multiplicative group by testing the order 
-- where it could produce 1 early.

primitiveTest charPoly@(Poly len _) =
    let divisors = divisorsOfPowerOf2 len
        powers   = map (toInt . powerOfX charPoly) divisors
    in notElem 1 powers

-- special case some small Mersenne primes, factorize everything else

divisorsOfPowerOf2 k =
    let n        = 2 ^ k - 1
    in if k `elem` [31,61,89,107,127,521,607]
       then [1]
       else map (quot n . fst) (factorize n)


-- Check that the polynomial contains no irreducible factors of degree less 
-- than len / 2.  Test that it is relatively prime to (x^2^i - x) for i from
-- 1 to len / 2.

irreducibleTest charPoly@(Poly len pp) =
    let numIters     = quot len 2
        x            = fromInt len 2
        fullCharPoly = Poly (len + 1) (True : pp)
    in irreducibleTest' fullCharPoly charPoly x x numIters 

irreducibleTest' _fullCharPoly _charPoly _ _ 0 = True

irreducibleTest' fullCharPoly charPoly x y i = 
    let newY = polyProduct charPoly y y
        test = polySum newY x
        gcd  = polyGCD fullCharPoly test
    in toInt gcd == 1 && irreducibleTest' fullCharPoly charPoly x newY (i - 1)
    
{------}{------}{------}{------}{------}{------}{------}{------}{------}{------}


smallHalf :: (Integral i) => i -> i -> [[Bool]]
smallHalf n k =
  filter (\xs -> xs >= reverse xs) $ map (subsetFromInteger n k) [0 .. (choose n k - 1)]

kWeightPolys :: (Integral i) => i -> i -> [Poly]
kWeightPolys len k = 
    map (fromBits . (True :)) (smallHalf (len - 1) k)

oddWeightPolys :: (Integral i) => i -> [Poly]
oddWeightPolys len = concatMap (kWeightPolys len) [1, 3 .. len - 1] 


getCharPoly :: (Integral i) => i -> Poly
getCharPoly = head . filter primitiveTest . filter irreducibleTest . oddWeightPolys
