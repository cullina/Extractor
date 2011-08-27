module RandomList where

import RandomValue

data RList a = RValue a (Maybe (a, RList a))