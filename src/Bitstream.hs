module Bitstream 
    (
     RState(..),
     Bitstream(..),
     getBit,
     getBitM,
     mapFst,
     stdBitstream,
     newBitstream,
     mapBitstream
    )
where

import Bit
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

generateList :: RState a -> Bitstream -> [a]

generateList m = unfoldr (Just . runState m)




generateFiniteList f n bs = 
    generateFiniteList' f n ([], bs)

generateFiniteList' f 0 p = p

generateFiniteList' f n (xs, bs) = 
    let (x, bs') = f bs
    in generateFiniteList' f (n - 1) (x:xs, bs')
   

 
generateFiniteListM :: RState a -> Int -> RState [a]

generateFiniteListM m n =
    generateFiniteListM' m n (return []) 

generateFiniteListM' :: RState a ->  Int -> RState [a] -> RState [a]

generateFiniteListM' m 0 mm = mm

generateFiniteListM' m n mm = 
    generateFiniteListM' m (n - 1) (liftM2 (:) m mm)

{--------}

mapBitstream :: (a -> b -> (c, b)) -> [a] -> b -> ([c], b)

mapBitstream f as bs = foldl' (foldHelper f) ([], bs) as


foldHelper :: (a -> b -> (c, b)) -> ([c], b) -> a -> ([c], b)

foldHelper f (cs, b) a = 
    let (c, newB) = f a b
    in (c:cs, newB)


{--------}
--Unbias

vNUnbias bs = let (b1, bs') = getBit bs
                  (b2, bs'') = getBit bs'
              in if b1 == b2
                 then vNUnbias bs''
                 else (b2, bs'')

vNUnbiasM = state vNUnbias


vNStream = newBitstream . generateList vNUnbiasM
