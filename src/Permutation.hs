module Permutation where

import Uniform
import Falling
import SubsetSelection
import Util(mapPair)
import Data.Function(on)
import Data.List(sort, sortBy, partition)

slowPerm []     = []
slowPerm (u:us) = u : map (bumpUp u) (slowPerm us)
  
slowUnperm _ []     = []
slowUnperm m (p:ps) = 
  p : map (bumpDown p) (slowUnperm (m - 1) ps)
    
bumpUp c x 
  | x >= c    = x + 1 
  | otherwise = x

bumpDown c x 
  | x > c     = x - 1 
  | otherwise = x

replace x y z
  | x == z    = y
  | otherwise = z

perm :: (Integral a) => [UnifNat a] -> [a]

perm [] = []
perm [UnifNat 1 0] = [0]
perm f@(UnifNat m _ : _) =
  let split = div m 2
      (bs, (xs, ys)) = disect f split
  in unpartition bs (perm xs) (map (split +) (perm ys))

--disect proves n! = \binom{n}{k} k! (n-k)!
disect us split = 
  let ds = decisions us split
      f  = map fst
      g  = mapPair (map snd) . partition fst
  in (f ds, g ds)


decisions [] _ = []
decisions (u:us) split = 
  let (b, v) = decision split u  
  in (b, v) : decisions us (if b then split - 1 else split)


cyclePerm _ []     = []
cyclePerm m (u:us) = u : map (replace u m) (cyclePerm (m - 1) us)
  
                   
permToFn :: (Integral a) => [a] -> a -> Maybe a
permToFn []    _  = Nothing
permToFn (n:_)  0 = Just n
permToFn (_:ns) m = permToFn ns (m-1) 

permute :: (Integral a) => [a] -> [b] -> [b]
permute p = map snd . sortBy (compare `on` fst) . zip p

inverse :: (Integral a) => [a] -> [a]
inverse p = permute p [0..]

compose :: (Integral a) => [a] -> [a] -> [a]
compose p = permute (inverse p)

allPermutations :: Int -> [[Int]]
allPermutations n = map perm $ allFallings n n
