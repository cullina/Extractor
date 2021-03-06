module SortingNetwork where

import Array
import Data.List(foldl', elemIndex, groupBy, mapAccumL, minimumBy, intercalate)
import Data.Function(on)
import Data.Maybe(fromJust)
import Bit(allBitStrings, showBits)
import Util(mapFst, mapSnd, mapPair, keepArg, dup, (...), minimumsSoFarBy)
import Permutation(permute, allPermutations)           

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

sortListSteps n net = map head . groupBy ((==) `on` snd) . zip [0..] . map elems . snd . sortArraySteps net . listArray (0,n-1)

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

display :: (Show a) => [a] -> IO ()
display = mapM_ print

display2 :: (Show a) => [a] -> IO ()
display2 = putStrLn . intercalate "\n" . map show

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
applyOrdering list = map (fixPair . mapPair (fromJust . flip elemIndex list))



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
        
net2 = [(1,4),(2,3),
        (2,4),(1,3),--find maxes of top
        (11,14),(12,13),
        (11,13),(12,14), --maxes of bottom
        (4,8),(7,11), --bridge
        (5,8),(7,10),
        (3,7),(8,12), --bridge
        (5,9),(6,10),
        (5,6),(9,10),
        (3,5),(10,12), --bridge
        (5,7),(8,10),
        (4,5),(10,11), --bridge
        (1,2),(3,4),
        (13,14),(11,12),
        (6,7),(8,9),(7,8)]
       
net3 = [(1,4),(2,3),
        (2,4),(1,3),--find maxes of top
        (11,14),(12,13),
        (11,13),(12,14)] --maxes of bottom
        
pNet = [[(4,8),(7,11),(5,8),(7,10),(5,6),(9,10),(3,5),(10,12)], --bridge
        [(5,9),(6,10)],
        [(3,7),(8,12)],
        [(5,7),(8,10)],
        [(4,5),(10,11)], --bridge
        [(6,7),(8,9)]]
        
net4 = [(1,2),(3,4),
        (13,14),(11,12),
        (7,8)]
       
randomNet p = net3 ++ concat (permute p pNet) ++ net4

testRandomNet p = length $ testNetwork (randomNet p) 16 bs

testAllNets k = minimumsSoFarBy (compare `on` snd) . map (keepArg testRandomNet) . take k $ allPermutations 6

net5 = [(0,4),(1,5),(2,6),(3,7),
        (0,1),(2,3),(4,5),(6,7),
        (1,4),(3,6),
        (0,2),(1,3),(4,6),(5,7),
        (2,4),(3,5),
        (1,2),(3,4),(5,6)]
       
graph6 =  [("0000","0010"),("0100","0110"),("0000","0100"),("0010","0110"),("0010","0100"),
           ("0001","0011"),("0101","0111"),("0001","0101"),("0011","0111"),("0011","0101"),
           ("1000","1010"),("1100","1110"),("1000","1100"),("1010","1110"),("1010","1100"),
           ("1001","1011"),("1101","1111"),("1001","1101"),("1011","1111"),("1011","1101"),
           
           ("0000","0001"),("0010","0011"),("0100","0101"),("0110","0111"), --first bit = 0
           ("0001","0010"),("0101","0110"),
           ("0100","0001"),("0101","0010"),("0011","0110"),
        
           ("1000","1001"),("1010","1011"),("1100","1101"),("1110","1111"), --first bit = 1
           ("1001","1010"),("1101","1110"),
           ("1100","1001"),("1101","1010"),("1011","1110"),
        
           ("0000","1000"),("0010","1010"),("0100","1100"),("0110","1110"), --last bit = 0
           ("0100","1000"),("0110","1010"),
           ("1000","0010"),("1010","0100"),("0110","1100"),
        
           ("0001","1001"),("0011","1011"),("0101","1101"),("0111","1111"), --last bit = 1
           ("0101","1001"),("0111","1011"),
           ("1001","0011"),("1011","0101"),("0111","1101"),
        
           ("0001","1000"),("0111","1110"),("0101","1010"),("1001","0010"),("1001","0100"),("0110","1011"),("0110","1101")]
       
vList6 = ["0000","0001","1000","0100","1001","0010","0011","1010","0101","1100","1101","0110","1011","0111","1110","1111"]

net6 = applyOrdering vList6 graph6

test6 = head $ fullTest net6 16

       
--       0
--     1   2
--   4   3   5 
-- 9   7   8   6
--  10  12  11
--    13  14
--      15

        
diamondNet = [( 0, 1),( 4, 9),( 0, 4),( 1, 9),( 1, 4),
              ( 2, 3),( 7,10),( 2, 7),( 3,10),( 3, 7),
              ( 5, 8),(12,13),( 5,12),( 8,13),( 8,12),
              ( 6,11),(14,15),( 6,14),(11,15),(11,14),
        
              ( 0, 2),( 5, 6),( 0, 5),( 2, 6),( 2, 5),
              ( 1, 3),( 8,11),( 1, 8),( 3,11),( 3, 8),
              ( 4, 7),(12,14),( 4,12),( 7,14),( 7,12),
              ( 9,10),(13,15),( 9,13),(10,15),(10,13)]

ds = outputs diamondNet 16

net7 = diamondNet ++ 
       [(2,4),(1,5),(10,14),(11,13),
        (1,2),(13,14),
        
        (3,9),(6,12),(3,6),(9,12),
        (4,8),(5,7),(7,11),(8,10),

        (3,4),(3,5),(10,12),(11,12),
        (4,5),(10,11),        
        (7,8),
        (6,7),(8,9)]
        

net8 = diamondNet ++ 
       [(1,5),(4,8),(3,6),
        (10,14),(9,12),(7,11),
        
        (2,4),(3,9),(5,7),
        (11,13),(8,10),(6,12),
                
        (1,2),(13,14),

        (3,5),(3,4),(4,5),
        (10,12),(11,12),(10,11),
        (6,7),(8,9),(7,8)]
        
       
--       0
--     1   2
--   4   3   5 
-- 6   8   7   9
--  10  12  11
--    13  14
--      15

        
diamondNet2 = [(0, 1),( 4, 6),( 0, 4),( 1, 6),( 1, 4),
              ( 2, 3),( 8,10),( 2, 8),( 3,10),( 3, 8),
              ( 5, 7),(12,13),( 5,12),( 7,13),( 7,12),
              ( 9,11),(14,15),( 9,14),(11,15),(11,14),
        
              ( 0, 2),( 5, 9),( 0, 5),( 2, 9),( 2, 5),
              ( 1, 3),( 7,11),( 1, 7),( 3,11),( 3, 7),
              ( 4, 8),(12,14),( 4,12),( 8,14),( 8,12),
              ( 6,10),(13,15),( 6,13),(10,15),(10,13)]

ds2 = outputs diamondNet2 16

net9 = diamondNet2 ++ 
       [(2,4),(1,5),(10,14),(11,13),
        (1,2),(13,14),
        
        (3,9),(6,12),(3,6),(9,12),
        (4,7),(5,8),(8,11),(7,10),

        (3,4),(3,5),(10,12),(11,12),
        (4,5),(10,11),
        (6,7),(8,9),        
        (7,8)]
