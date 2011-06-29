module Bitstream 
    (
     RState,
     Bitstream(..),
     getBit,
     getBitM,
     mapFst,
     stdBitstream,
     bitstreamFromInts,
     newBitstream,
     mapBitstream,
     useBitstream
    )
where

import Bit
import RandomValue
import System.Random
import Data.List(foldl', unfoldr)
import Control.Monad.State


--bitstreams
{------}{------}{------}{------}{------}{------}{------}{------}{------}{------}

type RState a = State Bitstream a

data Bitstream = Bitstream [Bool] Int

instance Show Bitstream where
    show (Bitstream bits n) = 
        "Bitstream " ++ show (take 10 bits) ++ show n

newBitstream x = Bitstream x 0


getBit (Bitstream [] n) = error ("Cannot getBit.  Bitstream is empty after " ++ 
                        show n ++ " bits were used from it.")

getBit (Bitstream (b:bs) n) = (b, Bitstream bs (n + 1))

getBitM = state getBit


mapFst f (x, y) = (f x, y)


bitstreamFromInts :: Int -> [Int] -> Bitstream

bitstreamFromInts w = 
    newBitstream . concatMap (intWToBits w [])

stdBitstream = 
    newBitstream . randomStream

randomStream = unfoldr (Just . random) . mkStdGen

{--------}

mapBitstream :: (a -> b -> (c, b)) -> [a] -> b -> ([c], b)

mapBitstream f as bs = foldl' (foldHelper f) ([], bs) as


foldHelper :: (a -> b -> (c, b)) -> ([c], b) -> a -> ([c], b)

foldHelper f (cs, b) a = 
    let (c, newB) = f a b
    in (c:cs, newB)

{--------}

useBitstream (Done x) bs = (x, bs)

useBitstream (NotDone f) bs = 
    let (b, bs') = getBit bs
    in useBitstream (f b) bs'