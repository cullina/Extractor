module UniversalHash where

import BinaryField
import PrimitivePoly
import Data.List(foldl')

basicHash input aBits bBits xBits = 
    let charPoly = getCharPoly input
        a        = Poly input aBits
        b        = Poly input bBits
        x        = Poly input xBits
    in expand input . polyBits . polySum b . polyProduct charPoly a $ x


alternate (a:b:c) = (a, b) : alternate c

alternate _ = []


universalHash abBits xBits =
    let input          = length xBits
        used           = take input abBits
        (aBits, bBits) = unzip used
        hashedBits = basicHash input aBits bBits xBits
    in used ++ alternate hashedBits
        

chainHash = foldl' universalHash

                     
