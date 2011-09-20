module Permutation where

import Uniform
import Falling
import SubsetSelection

slowPerm []     = []
slowPerm (u:us) = u : map (bumpUp u) (slowPerm us)
  
slowUnperm _ []     = []
slowUnperm m (p:ps) = 
  p : mapM (bumpDown p) (slowUnperm (m - 1) ps)
    
bumpUp c x 
  | x >= c    = x + 1 
  | otherwise = x

bumpDown c x 
  | x > c     = Just $ x - 1 
  | x == c    = Nothing
  | otherwise = Just x

replace x y z
  | x == z    = y
  | otherwise = z

perm :: (Integral a) => [UnifNat a] -> [a]

perm [] = []
perm [UnifNat 1 0] = [0]
perm f@(UnifNat m _ : _) =
  let split = div m 2
      (bs, xs, ys) = disect f split
  in unpartition bs (map (split +) (perm xs)) (perm ys)



disect us split = 
  let ds = decisions us split
      f = map fst
      g = map snd . filter fst
      h = map snd . filter (not . fst)
  in (f ds, g ds, h ds)


decisions [] _ = []
decisions (u:us) split = 
  let (b, v) = decision split u  
  in (b, v) : decisions us (if b then split else (split - 1))


cyclePerm _ []     = []
cyclePerm m (u:us) = u : map (replace u m) (cyclePerm (m - 1) us)
  
                   
permToFn :: (Integral a) => [a] -> a -> Maybe a
permToFn []    _  = Nothing
permToFn (n:_)  0 = Just n
permToFn (_:ns) m = permToFn ns (m-1) 
