module UniversalHash where

import Bitstream
import BinaryField
import PrimitivePoly
import SubsetSelection
import Data.List(foldl')
import Control.Monad(replicateM, liftM)


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


getSeed (x:_) = 
    let seedLength  = 2 * length x
    in replicateM seedLength getBitM

getSeed [] = undefined


chainHash :: [[Bool]] -> RState [Bool]

chainHash xx = 
    liftM (flip (foldl' universalHash) xx) (getSeed xx)


pickSubsets numGoodBits bits = 
    let len           = toInteger (length bits)
        sizes         = subsetSizes len numGoodBits
        picker        = liftM (map (getSubset bits))
        randomSubsets = mapM (subsetIncrementallyM len)
    in  picker (randomSubsets sizes)


extractor numGoodBits bits = chainHash =<< pickSubsets numGoodBits bits



subsetSizes :: Integer -> Integer -> [Integer]

subsetSizes = undefined
