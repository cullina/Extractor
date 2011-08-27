module Delannoy where

import Data.List(foldl', unfoldr)

data Step = Horizontal | Vertical | Diagonal deriving Eq

instance Show Step where
  show Horizontal = "_"
  show Vertical   = "|"
  show Diagonal   = "/"

allPaths 0 0 = [[]]
allPaths m 0 = map (Horizontal :) (allPaths (m - 1) 0)
allPaths 0 n = map (Vertical :)   (allPaths 0 (n - 1))
allPaths m n = map (Horizontal :) (allPaths (m - 1) n) ++
               map (Vertical :)   (allPaths m (n - 1)) ++
               map (Diagonal :)   (allPaths (m - 1) (n - 1))

toTuple Horizontal = (1, 0)
toTuple Vertical   = (0, 1)
toTuple Diagonal   = (1, 1)

displacement = foldl' addTuple (0, 0) . map toTuple
  where addTuple (a, b) (c, d) = (a + c, b + d)

dual Horizontal = Vertical
dual Vertical   = Horizontal
dual Diagonal   = Diagonal

translate slack [] = []
translate slack (Horizontal:ss) = translate (slack + 1) ss
translate slack (Vertical:ss)   = slack : translate 0 ss
translate slack (Diagonal:ss)   = -(slack + 1) : translate 0 ss

pathToPoint = foldr pTP (0, [])
  where pTP Horizontal (slack, xs) = (slack + 1, xs)
        pTP Vertical   (slack, xs) = (0, slack : xs)
        pTP Diagonal   (slack, xs) = (0, -(slack + 1) : xs)
        
pointToPath = unfoldr pTP
  where pTP (0, [])     = Nothing
        pTP (0, (x:xs)) = if x >= 0
                          then Just (Vertical, (x, xs))
                          else Just (Diagonal, (-x - 1, xs))
        pTP (x, xs)     = Just (Horizontal, (x - 1, xs))

params xs = (fst xs + (sum . map abs . snd) xs, (length . snd) xs)