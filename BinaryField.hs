module BinaryField where

import Bitstream(bitsToInt, intToBits, intWToBits)

data Poly = Poly {
      polyLen  :: Int 
    , polyBits :: [Bool]
    }

instance Show Poly where
    show p@(Poly len pp) =
        show (len, toInt p, map showBool (expand len pp))


showBool b = if b then 'X' else '_'

xor True = not
xor False = id


expand 0 _ = []

expand len [] = replicate len False

expand len (p:ps) = p : expand (len - 1) ps


prunePoly poly@(Poly l pp) =
    prunePoly' l pp

prunePoly' 0 _  = Poly 0 []

prunePoly' _ [] = Poly 0 []

prunePoly' l pp@(p:ps) =
    if p
    then Poly l pp
    else prunePoly' (l - 1) ps


toInt (Poly len pp) = bitsToInt $ expand len pp

fromBits bits = Poly (length bits) bits

fromInt len = fromBits . (intWToBits len [])

sparsePoly len = fromInt len . sum . map (2 ^) 

zeroPoly len = Poly len []

onePoly len = Poly len $ (replicate (len - 1) False) ++ [True]

allPolys len = map (fromBits . intWToBits len []) [1..(2 ^ len - 1)]


{-Multiply by x, then reduce by characteristic polynomial -}

timesX charPoly p@(Poly l []) = p

timesX charPoly (Poly l (a:as)) = 
    if a
    then polySum charPoly (Poly l as)
    else (Poly l as)


allPowersOfX charPoly n =
    allPowersOfX' charPoly n (onePoly (polyLen charPoly))

allPowersOfX' charPoly 0 x = [x]

allPowersOfX' charPoly n x =
    x : allPowersOfX' charPoly (n - 1) (timesX charPoly x)


powerOfX charPoly n = 
    let square x = polyProduct charPoly x x
        times    = timesX charPoly
        bits     = intToBits [] n
        one      = onePoly (polyLen charPoly)
    in power square times bits one

powerOfY charPoly n y = 
    let square x = polyProduct charPoly x x
        times    = polyProduct charPoly y
        bits     = intToBits [] n
        one      = onePoly (polyLen charPoly)
    in power square times bits one



power square times [] accum = accum

power square times (b:bs) accum =
    let accum2 = square accum 
        accum3 = if b
                 then times accum2
                 else accum2
    in power square times bs accum3





polySum (Poly la pa) (Poly lb pb) = 
    if la == lb
    then Poly la (polySum' pa pb)
    else error "Polynomial length mismatch."

polySum' (a:as) (b:bs) = xor a b : polySum' as bs
polySum' []     bb     = bb
polySum' aa     []     = aa 
    


polyProduct charPoly@(Poly l p) a@(Poly la pa) b@(Poly lb pb) = 
    if l == la && l == lb
    then polyProduct' charPoly a (expand l pb) (zeroPoly l)
    else error "Polynomial length mismatch."


polyProduct' charPoly a [] accum = accum

polyProduct' charPoly a (b:bs) accum = 
    let accum2 = timesX charPoly accum
        accum3 = if b
                 then polySum a accum2
                 else accum2
    in polyProduct' charPoly a bs accum3


polyRem a b =
    polyRem' (prunePoly a) (prunePoly b)
   

polyRem' a@(Poly la pa) b@(Poly lb pb) =
    if lb > la
    then a
    else let aa = prunePoly $ polySum a $ Poly la pb
         in polyRem' aa b


polyGCD a b = 
    if 0 == toInt b
    then a
    else polyGCD b $ polyRem a b


embedPoly newLength (Poly length p) = 
    Poly newLength (replicate (newLength - length) False ++ p)

