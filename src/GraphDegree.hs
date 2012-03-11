module GraphDegree where

import Graph
import Data.List(delete, minimumBy, maximumBy, unfoldr)
import Bit(showBits)
import Util(mapFst, mapSnd)
import Data.Function(on)

degeneracy :: Eq a => FullAdj a -> Int
degeneracy = maximum . map snd . degenSequence

degenSequence :: Eq a => FullAdj a -> [(a,Int)]
degenSequence = unfoldr dS
  where dS (FullAdj []) = Nothing
        dS g =  
          let minDegree = minimumBy (compare `on` snd) . degrees 
              (v, n) = minDegree g
          in Just ((v,n), deleteVertex v g)

deleteVertex :: Eq a => a -> FullAdj a -> FullAdj a
deleteVertex v = FullAdj . dV v . fromFullAdj
  where 
    dV _ [] = []
    dV v ((x,ys):es)
      | v == x    = dV v es
      | otherwise = (x , delete v ys) : dV v es

degrees = map (mapSnd length) . fromFullAdj

degreeData g =
  let ds = degrees g
      minim = mapFst showBits $ minimumBy (compare `on` snd) ds
      maxim = mapFst showBits $ maximumBy (compare `on` snd) ds
      n = length ds
      m = sum $ map snd ds
      av = fromIntegral m / fromIntegral n
      sparsity = fromIntegral m / fromIntegral (n * (n-1))
  in (n, minim, av, sparsity, log sparsity, maxim, degeneracy g)
     
argmaxDegree = mapFst showBits . maximumBy (compare `on` snd) . degrees

argminDegree = mapFst showBits . minimumBy (compare `on` snd) . degrees

maxDegree = maximum . map snd . degrees
minDegree = minimum . map snd . degrees
totalDegree = sum . map snd . degrees
