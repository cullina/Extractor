
{- 7: x^2 + x + 1 -}
p2 = [True, True]

{- 13: x^3 + x + 1 -}
p3 = [False, True, True]

{- 23: x^4 + x + 1 -}
p4 = [False, False, True, True]

{- 45: x^5 + x^2 + 1 -}
p5 = [False, False, True, False, True]

{- 103: x^6 + x + 1 -}
p6 = [False, False, False, False, True, True]

{- 211: x^7 + x^3 + 1 -}
p7 = [False, False, False, True, False, False, True]

{- 435 x^8 + x^4 + x^3 + x^2 + 1 -}
p8 = [False, False, False, True, True, True, False, True]

{- 1021, 2011, -}


xor x y = (x && not y) || (not x && y)


{-Multiply by x, then reduce by characteristic polynomial -}

reduce charPoly (p:ps) = let shifted = ps ++ [False]
                         in if p
                            then polySum charPoly shifted
                            else shifted

polySum = zipWith xor

polyProduct :: [Bool] -> [Bool] -> [Bool] -> [Bool]

polyProduct charPoly q [] = []

polyProduct charPoly q (p:ps) = if p
                                then polyProduct' charPoly q ps q
                                else polyProduct charPoly q ps 

polyProduct' :: [Bool] -> [Bool] -> [Bool] -> [Bool] -> [Bool]

polyProduct' charPoly q [] accum = accum

polyProduct' charPoly q (p:ps) accum = let accum2 = reduce charPoly accum
                                           accum3 = if p
                                                    then polySum q accum2
                                                    else accum2
                                       in polyProduct' charPoly q ps accum3

universalHash charPoly outputSize a b = take outputSize $ polySum b $ polyProduct a

randomHash random (charPoly outputSize x) = let (a, b) = split (length charPoly) random
                                          in universalHash charPoly outputSize a b x

chainHash random xs = foldl randomHash random xs




getCharPoly len = 
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
    | otherwise = []


