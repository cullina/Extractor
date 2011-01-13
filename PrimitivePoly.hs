module PrimitivePoly where

import Prime
import BinaryField
import SubsetSelection


-- Check that x generates the whole multiplicative group by testing the order 
-- where it could produce 1 early.

primitiveTest charPoly@(Poly len pp) =
    let divisors = divisorsOfPowerOf2 len
        powers   = map (toInt . powerOfX charPoly) divisors
    in all ((/=) 1) powers

-- special case some small Mersenne primes, factorize everything else

divisorsOfPowerOf2 k =
    let n        = 2 ^ k - 1
    in if or $ map (k ==) [31,61,89,107,127,521,607]
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

irreducibleTest' fullCharPoly charPoly x y 0 = True

irreducibleTest' fullCharPoly charPoly x y i = 
    let newY = polyProduct charPoly y y
        test = polySum newY x
        gcd  = polyGCD fullCharPoly test
    in if toInt gcd == 1
       then irreducibleTest' fullCharPoly charPoly x newY (i - 1) 
       else False
    
{------}{------}{------}{------}{------}{------}{------}{------}{------}{------}

-- list ranges from [0 , max) 

splitInHalf list =
    let (q, r) = quotRem (length list) 2
        (a, b) = splitAt q list
        (c, d) = splitAt r b
    in (a, c, d)


testBalance max list = 
    let (a, b, c) = splitInHalf list
        folded    = map (2 *) b ++ zipWith (+) (reverse a) c
    in folded <= repeat (max - 1)


{------}{------}{------}{------}{------}{------}{------}{------}{------}{------}

smallHalf n k =
    filter (testBalance n) $ map (subsetFromInteger n k) $ [0 .. (choose n k - 1)]


kWeightPoly len k = 
    map (sparsePoly len . (0 :) . (map (1 +))) (smallHalf (len - 1) k)


oddWeightPolys len = concatMap (kWeightPoly len) [1, 3 .. len - 1] 

getCharPoly :: Int -> Poly

getCharPoly = head . filter primitiveTest . filter irreducibleTest . oddWeightPolys
