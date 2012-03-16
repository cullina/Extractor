module Main where

import System.Environment(getArgs)
import Data.List(sort)
import ExactCover
import QaryGraphs



main = do
  [q,n] <- fmap (map read) getArgs
  mapM_ print . sort . map sort . solveEC . fromBottom $ qInsertionStars q n

