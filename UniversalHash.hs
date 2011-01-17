module UniversalHash where

import Bitstream
import BinaryField
import PrimitivePoly
import SubsetSelection
import Data.List(foldl')
import Control.Monad(mapM)


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
        

chainHash xx@(x:xs) bs = 
    let seedLength  = 2 * length x
        (seed, bs') = generateFiniteList getBit seedLength bs
    in (foldl' universalHash (alternate seed) xx, bs')

pickSubsets bits = 
    mapM (subsetIncrementallyM (length bits)) >>= liftM (map (indicesToSubset bits))

{-
subsetSizes (x:xs) (p, q) bound =
    let next = 
    if 
    

extractor bits = pickSubsets bits >>= chainHash . pickSub

-}