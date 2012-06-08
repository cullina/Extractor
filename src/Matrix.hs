module Matrix where

import Data.Monoid
import Data.Tuple(swap)

data BMat n = BMat ((Bool, Bool) -> n)

instance Num n => Monoid (BMat n) where
  mempty                    = BMat $ lookupBB (1, 0, 0, 1)
  mappend (BMat x) (BMat y) = BMat $ bbMatrixMult x y

instance Show n => Show (BMat n) where
  show (BMat x) = show (expandBB x)

lookupBB :: (n, n, n, n) -> (Bool,Bool) -> n
lookupBB (a,_,_,_) (False, False) = a
lookupBB (_,b,_,_) (False, True)  = b
lookupBB (_,_,c,_) (True,  False) = c
lookupBB (_,_,_,d) (True,  True)  = d

expandBB :: ((Bool, Bool) -> n) -> (n,n,n,n)
expandBB f = (f (False,False), f (False,True), f (True,False), f (True,True))

entryMult :: Num n => ((a, Bool) -> n) -> ((Bool, b) -> n) -> a -> b -> Bool -> n
entryMult x y i j k = x (i, k) * y (k, j) 

bbMatrixMult :: Num n => ((a, Bool) -> n) -> ((Bool, b) -> n) -> (a, b) -> n
bbMatrixMult x y (i, j) = f False + f True
  where f = entryMult x y i j

transpose :: ((a, b) -> n) -> (b, a) -> n
transpose m = m . swap

sumVec :: Num n => (Bool, ()) -> n
sumVec (False, ()) = 1 
sumVec (True,  ()) = 1

diffVec :: Num n => (Bool, ()) -> n
diffVec (False, ()) = 1 
diffVec (True,  ()) = -1
