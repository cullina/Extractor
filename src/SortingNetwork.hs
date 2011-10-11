module SortingNetwork where

import Array
import Data.List(foldl', elemIndex)
import Data.Maybe(fromJust)
import Bit(allBitStrings, showBits)
import Util(mapFst, mapSnd, mapPair, keepArg)
           

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

increasing :: (Ord a) => [a] -> Bool
increasing [] = True
increasing xss@(_:xs) = 
  and (zipWith (<=) xss xs) 

testNetwork net n = filter (not . snd . snd) . map (mapSnd (f increasing) . f (sortList n net)) $ allBitStrings n
                    where f g = mapFst showBits . keepArg g

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

graph = [ ("0000","0001")
        , ("0010","0011")
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
        , ("0111","1111")
          
        , ("1001","1100")
        , ("0110","0011")
        , ("1001","0011")
        , ("0110","1100")
        
        , ("1001","0101")
        , ("0110","1010")
        , ("1001","1010")
        , ("0110","0101")
        
        , ("0000","0000")
        , ("0000","0000")
        , ("0000","0000")
        , ("0000","0000")
        , ("0000","0000")
        , ("0000","0000")
        , ("0000","0000")

        , ("0000","0000")
        , ("0000","0000")
        , ("0000","0000")
                    
        , ("0000","0000")
        , ("0000","0000")
        , ("0000","0000")
          
        , ("0000","0000")
        , ("0000","0000")
        
        , ("1000","0010")
        , ("1110","1011")
        , ("0000","0000")
        , ("0000","0000")
          
        , ("1000","0001")
        , ("0100","0010")
        , ("1110","0111")
        , ("1101","1011")
          
        , ("0101","0011")
        , ("1010","1100")
        , ("0101","1010")
]