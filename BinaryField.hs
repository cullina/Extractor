module BinaryField where

import Bitstream(bitsToInt, intToBits, intWToBits)

data Poly = Poly {
      polyLen  :: Int 
    , polyBits :: [Bool]
    }

instance Show Poly where
    show (Poly len pp) =
        map showBool (expand len pp)


showBool b = if b then 'X' else '_'

xor True = not
xor False = id


nextTerm (p:ps) = (p, ps)
nextTerm [] = (False, [])


expand 0 pp = []

expand len pp =
    let (p, ps) = nextTerm pp
    in p : expand (len - 1) ps


toInt (Poly len pp) = bitsToInt $ expand len pp

fromBits bits = Poly (length bits) bits


zeroPoly len = Poly len []

onePoly len = Poly len $ (replicate (len - 1) False) ++ [True]

allPolys len = map (fromBits . intWToBits len []) [1..(2 ^ len - 1)]


{-Multiply by x, then reduce by characteristic polynomial -}

timesX charPoly (Poly l pa) = 
    let (a, as) = nextTerm pa
        shifted = (Poly l as)
    in if a
       then polySum charPoly shifted
       else shifted


allPowersOfX charPoly n =
    allPowersOfX' charPoly n (onePoly (polyLen charPoly))

allPowersOfX' charPoly 0 x = [x]

allPowersOfX' charPoly n x =
    x : allPowersOfX' charPoly (n - 1) (timesX charPoly x)


powerOfX charPoly n = 
    powerOfX' charPoly (intToBits [] n) (onePoly (polyLen charPoly))

powerOfX' charPoly [] accum = accum

powerOfX' charPoly (b:bs) accum =
    let accum2 = polyProduct charPoly accum accum 
        accum3 = if b
                 then timesX charPoly accum2
                 else accum2
    in powerOfX' charPoly bs accum3


polySum (Poly la pa) (Poly lb pb) = 
    if la == lb
    then Poly la (zipWith xor (expand la pa) (expand lb pb))
    else error "Polynomial length mismatch."

    


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
    in polyProduct' charPoly a (len - 1) bs accum3


--polyQuotRem a@

--polyGCD 


resizePoly newLength (Poly _ p) =
    Poly newLength (take newLength p)

embedPoly newLength (Poly length p) = 
    Poly newLength (replicate (newLength - length) False ++ p)