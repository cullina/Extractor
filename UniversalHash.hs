module UniversalHash where

import BinaryField


universalHash input output a b x = 
    let charPoly = getCharPoly input 
    in truncatePoly output . polySum b . polyProduct charPoly a

{-
randomHash random (charPoly, outputSize, x) = let (a, b) = split (length charPoly) random
                                              in universalHash charPoly outputSize a b x

chainHash random xs = foldl randomHash random xs
-}



getCharPoly len
    | len ==  2 = p2
    | len ==  3 = p3
    | len ==  4 = p4
    | len ==  5 = p5
    | len ==  6 = p6
    | len ==  7 = p7
    | len ==  8 = p8

{-
    | len ==  9 = p9
    | len == 10 = p10
    | len == 11 = p11
    | len == 12 = p12
    | len == 13 = p13
    | len == 14 = p14
    | len == 15 = p15
    | len == 16 = p16
-}  
    | otherwise = undefined


