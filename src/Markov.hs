module Markov where

import Bitstream
import Control.Monad.State

type MarkovState a = State (a, Bitstream) a


data Markov a = Markov (RState a) (a -> RState a)


convertToMarkov :: (a -> RState a) -> MarkovState a

convertToMarkov transition =  
    state $ duplicateFst . uncurry (runState . transition)
        where duplicateFst (x,s) = (x,(x,s))


getChain transition n =
    replicate n (convertToMarkov transition)