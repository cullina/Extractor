module Permutation where

import Uniform
import SubsetSelection(unpartition)


data Falling a = Falling a [a]
               deriving Show


split (Falling m (x:xs)) = Just (UnifNat m x, Falling (m - 1) xs) 
split (Falling m [])     = Nothing


slowPerm f = case split f of
  Nothing -> []
  Just (UnifNat _ u, us) -> 
    u : map (\x -> if x >= u then x + 1 else x) (slowPerm us)
  
  
perm [] = []
perm us@(UnifNat 1 _ : _) = [0]
perm us@(UnifNat max _ : _) =
  let split = div max 2 
      ds = disect us split
      xs = map (+ split) . perm . map snd . filter fst $ ds
      ys = perm . map snd . filter (not . fst) $ ds
  in unpartition (map fst ds) xs ys

disect [] _ = []
disect (u:us) split = 
  let (b, v) = decision split u  
  in (b, v) : disect us (if b then split else (split - 1))
  

slowUnperm m ps = Falling m $ slowUnperm' m ps where
  slowUnperm' _ []     = []
  slowUnperm' m (p:ps) = 
    p : map (\x -> if x > p then x - 1 else x) (slowUnperm' (m - 1) ps)