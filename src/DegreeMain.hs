module Main where

import System.Environment(getArgs)
import LevOps

main = do
  [s,m,n] <- fmap (map read) getArgs
  mapM_ (print . maxDegree s) [m..n]
