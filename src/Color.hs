module Color where

import qualified Data.IntMap as IM
import Graph
import Data.Foldable(foldrM)
import Data.Maybe(catMaybes)
import ListSet(listSetFromList, asymDiff)

data Coloring = Coloring 
    { numUsed  :: Int
    , coloring :: IM.IntMap Int
    } deriving Show
  

emptyColoring :: Coloring
emptyColoring = Coloring 0 IM.empty
     
colorVertex :: Coloring -> Int -> Int -> Coloring
colorVertex (Coloring used cMap) v c = 
  Coloring (max used (c + 1)) (IM.insert v c cMap)

extendColoring :: Int -> (Int, [Int]) -> Coloring -> [Coloring]
extendColoring maxColor (u, vs) c@(Coloring used cMap) = 
  let adjacentColors = listSetFromList . catMaybes $ map (`IM.lookup` cMap) vs
  in map (colorVertex c u) $ asymDiff [0 .. (min used maxColor)] adjacentColors

     
color :: Int -> FwdAdj Int -> [Coloring]
color n = foldrM (extendColoring (n-1)) emptyColoring . fromFwdAdj


testEdge :: IM.IntMap Int -> (Int, Int) -> Bool
testEdge c (x,y) = 
  case (IM.lookup x c, IM.lookup y c) of
    (Just xx, Just yy) -> xx /= yy
    (_, _)             -> False

testColoring :: Coloring -> EdgeList Int -> Bool
testColoring c = all (testEdge (coloring c)) . fromEdgeList

colorClasses :: Coloring -> [(Int, [Int])]
colorClasses (Coloring n cMap) = 
  let as = IM.assocs cMap
      f k = (k, map fst $ filter ((==) k . snd) as)
  in map f [0 .. n - 1]
      
