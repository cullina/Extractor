module PrimitivePoly where

import Prime
import BinaryField


primitiveTest charPoly@(Poly len pp) =
    let n        = 2 ^ len - 1
        divisors = testDivisors n primes
        powers   = map (toInt . (powerOfX charPoly) . (quot n)) divisors
    in all ((/=) 1) powers



t = True
f = False

{- 7: x^2 + x + 1 -}
p2 = Poly 2 [t, t]

{- 13: x^3 + x + 1 -}
p3 = Poly 3 [f, t, t]

{- 23: x^4 + x + 1 -}
p4 = Poly 4 [f, f, t, t]

{- 45: x^5 + x^2 + 1 -}
p5 = Poly 5 [f, f, t, f, t]

{- 103: x^6 + x + 1 -}
p6 = Poly 6 [f, f, f, f, t, t]

{- 211: x^7 + x^3 + 1 -}
p7 = Poly 7 [f, f, f, t, f, f, t]

{- 435 x^8 + x^4 + x^3 + x^2 + 1 -}
p8 = Poly 8 [f, f, f, t, t, t, f, t]

{- 1021, 2011, -}


