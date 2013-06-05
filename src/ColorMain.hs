module Main where

import System.Environment(getArgs)
import Color
import GraphAlgorithms(allColorings, allColoringsL)
import QaryGraphs
import Graph(adjList, adjListFull, EdgeList)
import Util(printLength, incrementalLength)


main = getArgs >>= f

f :: [String] -> IO [()]
f [a,b,q,n,k] = output a b (read k) $ qIntEdges (read q) (read n)
f _ = error "Arguments: algorithm, output type, q, n, k"

output :: String -> String -> Int -> EdgeList Int -> IO [()]
output a
  | a == "-1" = output1
  | a == "-2" = output2
  | a == "-3" = output3
  | otherwise = error "First option should be -1,-2,or -3"
                
output1 b k
  | b == "-l"  = printLength . color k . adjList
  | b == "-il" = incrementalLength . color k . adjList
  | b == "-c"  = mapM (print . colorClasses) . color k . adjList
  | otherwise  = error "Second option should be -l, -il, -c"

output2 b k
  | b == "-l"  = printLength . allColorings k . adjList
  | b == "-il" = incrementalLength . allColorings k . adjList
  | b == "-c"  = mapM print . allColorings k . adjList
  | otherwise  = error "Second option should be -l, -il, -c"

output3 b k
  | b == "-l"  = printLength . allColoringsL k . adjListFull
  | b == "-il" = incrementalLength . allColoringsL k . adjListFull
  | b == "-c"  = mapM print . allColoringsL k . adjListFull
  | otherwise  = error "Second option should be -l, -il, -c"

{-
main = do
  args    <- getArgs
  [a,b]   <- fmap (take 2) args
  [q,n,k] <- fmap (map read . drop 2) args
--  mapM_ (print . colorClasses) . color k . adjList $ qIntEdges q n
  incrementalLength . color k . adjList $ qIntEdges q n
-}
