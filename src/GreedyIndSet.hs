module GreedyIndSet where

import Graph(ArrayGraph(..), Subgraph(..))
import Data.IntMap(IntMap,(!))
import Data.IntSet(IntSet, member, delete)
import Data.List(foldl')
import Util(mapFst)

remainingNeighbors :: IntMap [Int] -> IntSet -> Int -> [Int] 
remainingNeighbors graph vs = filter (flip member vs) . (graph !)

updateSubgraph :: IntMap [Int] -> Subgraph -> Int -> Subgraph
updateSubgraph graph (Subgraph vs ds) v =
  let rn    = remainingNeighbors graph vs v
      newVs = foldl' (flip delete) vs (v:rn)
      newDs = filter (flip member newVs . fst) ds
      reduced = map (map (\x -> (x,-1)) . remainingNeighbors graph newVs) rn
      newestDs = foldr addListIntMaps [] (newDs : reduced) 
  in Subgraph newVs newestDs
  
minDegs :: [(Int,Int)] -> [Int]
minDegs [] = []
minDegs ((x,deg):xs) = fst $ foldr mins ([x],deg) xs 
  where
    mins :: (Int,Int) -> ([Int],Int) -> ([Int],Int)
    mins (v, m) (vs, n) = 
      case compare m n of
        GT -> (vs, n)
        EQ -> (v : vs, n)
        LT -> ([v], m)
        
addListIntMaps :: [(Int,Int)] -> [(Int,Int)] -> [(Int,Int)]
addListIntMaps xs [] = xs
addListIntMaps [] ys = ys
addListIntMaps xx@((x,m):xs) yy@((y,n):ys) = 
  case compare x y of
    EQ -> (x,m+n) : addListIntMaps xs ys
    LT -> (x,m) : addListIntMaps xs yy
    GT -> (y,n) : addListIntMaps xx ys

        
greedyIndSet :: ArrayGraph -> [Int]
greedyIndSet (ArrayGraph graph sg) = gis sg
  where
    gis sg@(Subgraph _ ds) =
      case minDegs ds of
        []    -> []
        (x:_) -> x : gis (updateSubgraph graph sg x)
        
greedyIndSets :: ArrayGraph -> [[Int]]
greedyIndSets (ArrayGraph graph sg) = gis sg
  where
    f sg x = map (x :) (gis (updateSubgraph graph sg x))
    gis (Subgraph _ ds) =     
      case minDegs ds of
        [] -> [[]]
        xs -> concatMap (f sg) xs

greedyIndSetN :: Int -> ArrayGraph -> ([Int],Int)
greedyIndSetN n (ArrayGraph graph sg) = gis n sg
  where
    gis n sg@(Subgraph _ ds) =
      case minDegs ds of
        [] -> ([],n)
        xs -> let (x, newN) = rListElem xs n
              in mapFst (x :) (gis newN (updateSubgraph graph sg x))

greedyIndSetFirstN :: Int -> ArrayGraph -> [[Int]]
greedyIndSetFirstN n g = map fst . filter ((0 ==) . snd) $ map (flip greedyIndSetN g) [0..n-1]



rListElem :: [a] -> Int -> (a,Int)
rListElem xs n =
  let (q,r) = quotRem n (length xs)
  in (xs !! r, q)