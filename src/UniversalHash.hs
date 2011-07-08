module UniversalHashR where

import BinaryField
import PrimitivePoly
import SubsetSelection(getSubset)
import RandomValue(useFixedNumber)
import RandomSubset
import Data.List(foldl')
import Control.Monad(liftM, liftM2)


basicHash input aBits bBits xBits = 
    let charPoly = getCharPoly input
        a        = Poly input aBits
        b        = Poly input bBits
        x        = Poly input xBits
    in expand input . polyBits . polySum b . polyProduct charPoly a $ x


universalHash abBits xBits =
    let input          = length xBits
        (aBits, rest)  = splitAt input abBits
        bBits          = take input rest
        hashedBits     = basicHash input aBits bBits xBits
    in aBits ++ bBits ++ hashedBits


pickSubsets len subsetSizes bits = 
    let randomSubset = liftM (getSubset bits) . subsetIncrementally len
    in  mapM randomSubset subsetSizes


extractor goodBits bits = 
    let totalBits                 = fromIntegral $ length bits
        (seedLength, subsetSizes) = computeSubsetSizes goodBits totalBits
        seed                      = useFixedNumber $ fromIntegral seedLength
        subsets                   = pickSubsets totalBits subsetSizes bits
    in liftM2 (foldl' universalHash) seed subsets



computeSubsetSizes :: Integer -> Integer -> (Integer, [Integer])

computeSubsetSizes = undefined
