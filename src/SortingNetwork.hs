module SortingNetwork where

import Array
import Data.List(foldl', elemIndex, group, mapAccumL)
import Data.Maybe(fromJust)
import Bit(allBitStrings, showBits)
import Util(mapFst, mapSnd, mapPair, keepArg, dup, (...))
           

sortPair :: (Ix a, Ord b) => Array a b -> (a,a) -> Array a b
sortPair arr (x, y) = 
  let ax = arr ! x
      ay = arr ! y
  in if ax <= ay
     then arr
     else arr // [(x,ay), (y,ax)]
          
sortArray :: (Ix a, Ord b) => [(a,a)] -> Array a b -> Array a b
sortArray net arr = foldl' sortPair arr net   

sortList :: (Ix a, Num a, Ord b) => a -> [(a,a)] -> [b] -> [b]
sortList n net = elems . sortArray net . listArray (0,n-1) 

sortArraySteps net arr = mapAccumL (dup ... sortPair) arr net

sortListSteps n net = map head . group . map elems . snd . sortArraySteps net . listArray (0,n-1)

checkPair arr (x, y) =
  arr ! x <= arr ! y

checkArray :: (Ix a, Ord b) => [(a,a)] -> Array a b -> Bool
checkArray net arr = all (checkPair arr) net

checkList :: (Ix a, Num a, Ord b) => a -> [(a,a)] -> [b] -> Bool
checkList n net = checkArray net . listArray (0,n-1)

outputs net n = filter (checkList n net) (allBitStrings n)

increasing :: (Ord a) => [a] -> Bool
increasing [] = True
increasing xss@(_:xs) = 
  and (zipWith (<=) xss xs) 

testNetwork net n bs = filter (not . snd . snd) . map (mapSnd (f increasing) . f (sortList n net)) $ bs
                       where f g = mapFst showBits . keepArg g

fullTest net n = testNetwork net n $ allBitStrings n


delNet  = cubeNet 3 ++ [(1,2),(5,6),(1,4),(2,5),(3,6),(2,4),(3,5)]
delNet' = bindNet fourNet 4 ++ [(1,4),(2,5),(3,6),(2,4),(3,5)]

slide n = map (mapPair (n +))

cubeNet 0 = []
cubeNet n = bindNet (cubeNet(n - 1)) (2^(n-1))     

fourNet = cubeNet 2 ++ [(1,2)]

bindNet net n = net ++ slide n net ++ map (keepArg (n +)) [0..n-1]

newNet = [(0,1),(2,4),(3,5),(6,7),(0,2),(1,4),(3,6),(5,7),(0,3),(1,5),(2,6),(4,7),(1,3),(4,6),(2,3),(4,5),(1,2),(3,4),(5,6)]

fixPair (a,b) = if a > b then (b,a) else (a,b)

applyOrdering :: (Eq a) => [a] -> [(a,a)] -> [(Int, Int)]
applyOrdering list net = map (fixPair . mapPair (fromJust . (flip elemIndex) list)) net



vList = ["0000","0001","1000","0010","0100","1001","0011","0101","1010","1100","0110","1011","1101","0111","1110","1111"]

hNet = applyOrdering vList hGraph
net  = applyOrdering vList graph
bs = outputs hNet 16

hGraph =[ ("0000","0001")
        , ("0010","0011")
        , ("0100","0101")
        , ("0110","0111")
        , ("1000","1001")
        , ("1010","1011")
        , ("1100","1101")
        , ("1110","1111")
          
        , ("0000","0010")
        , ("0001","0011")
        , ("0100","0110")
        , ("0101","0111")
        , ("1000","1010")
        , ("1001","1011")
        , ("1100","1110")
        , ("1101","1111")
          
        , ("0000","0100")
        , ("0001","0101")
        , ("0010","0110")
        , ("0011","0111")
        , ("1000","1100")
        , ("1001","1101")
        , ("1010","1110")
        , ("1011","1111")
          
        , ("0000","1000")
        , ("0001","1001")
        , ("0010","1010")
        , ("0011","1011")
        , ("0100","1100")
        , ("0101","1101")
        , ("0110","1110")
        , ("0111","1111")]
          
          --middle square
graph = [ ("1001","1100")
        , ("0110","0011")
        
        , ("1001","0101")
        , ("0110","1010")
                         
        , ("1001","0011")
        , ("0110","1100")

          --middle/outer
        , ("0101","0010")
        , ("1010","0100")
        , ("0101","1011")
        , ("1010","1101")
                  
          --top/bottom internal
        , ("1000","0100")
        , ("0001","0010")
        , ("0111","1011")
        , ("1110","1101")
                  
        , ("1000","0010")
        , ("0001","0100")
        , ("0111","1101")
        , ("1110","1011")

          --middle/outer
        , ("1001","0010")
        , ("1001","0100")
        , ("0110","1011")
        , ("0110","1101")
        
        , ("1001","1010")
        , ("0110","0101")

        , ("1000","0001")
        , ("0100","0010")
        , ("1110","0111")
        , ("1101","1011")
          
        , ("0101","0011")
        , ("1010","1100")
        , ("0101","1010")]
        
net2 = [(1,4),
        (4,8),
        (2,4),(1,3),(1,2),--find max of top
        (11,14),
        (7,11),
        (11,13),(12,14),(13,14), --max of bottom
        (5,8),(7,10),
        (3,7),(8,12),
        (5,9),(6,10),
        (3,5),(10,12),
        (5,7),(8,10),
        (5,6),(9,10),
        (4,5),(10,11),
        (2,3),(3,4),
        (12,13),(11,12),
        (6,7),(8,9),(7,8)]