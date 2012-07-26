module Main where

import System.Environment(getArgs)
import LevOps

main = do
  [m,n] <- fmap (map read) getArgs
  mapM_ (print . maxDegree 2) [m..n]
