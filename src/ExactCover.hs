module ExactCover where

import Util(mapFst, mapSnd)
import Data.Function(on)
import Data.List(minimumBy, sort)
import Data.Maybe(catMaybes)
import Control.Monad((<=<))
import qualified Data.IntSet as IS
import qualified Data.IntMap as IM

data ECProblem = ECProblem (IM.IntMap IS.IntSet) (IM.IntMap IS.IntSet) deriving Show

solveEC :: ECProblem -> [[Int]]
solveEC p@(ECProblem _ bottom) =
  let f y = map (y :) . solveEC $ addToCover p y
  in case IM.assocs bottom of
      [] -> [[]]
      xs -> f <=< IS.elems . minimumBy (compare `on` IS.size) . map snd $ xs
    
addToCover :: ECProblem -> Int -> ECProblem
addToCover (ECProblem top bottom) n =
  let covered     = top IM.! n
      coveredList = IS.elems covered
      unusable    = IS.unions . catMaybes $ map (`IM.lookup` bottom) coveredList
      interTop    = foldr IM.delete top $ IS.elems unusable
      interBottom = foldr IM.delete bottom coveredList
      newTop      = IM.map (`IS.difference` covered) interTop
      newBottom   = IM.map (`IS.difference` unusable) interBottom
  in ECProblem newTop newBottom
     
transpose :: (Ord a, Ord b, Eq b) => [(a, [b])] -> [(b, [a])] 
transpose = collect . sort . concatMap expand
  where expand (x, ys) = map (\z -> (z, x)) ys 
        collect [] = []
        collect xs@((x, _) : _) = 
          let (ys, zs) = c x xs
          in (x, ys) : collect zs
        c _ [] = ([], [])
        c x ((y,z):ys) = 
          if x == y
          then mapFst (z :) (c x ys)
          else ([], (y,z):ys)
        
fromBottom :: [(Int, [Int])] -> ECProblem
fromBottom bs = ECProblem t b
  where (t, b) = fromOneSide bs
        
fromTop :: [(Int, [Int])] -> ECProblem
fromTop bs = ECProblem t b
  where (b, t) = fromOneSide bs

fromOneSide bs = (a, b) 
  where
    a = IM.fromList . map (mapSnd IS.fromList) $ transpose bs
    b = IM.fromList . map (mapSnd IS.fromList) $ bs

perfectColoring = solveEC . fromTop . zip [0..] . solveEC . fromBottom