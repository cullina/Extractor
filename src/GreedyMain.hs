module Main where

import GreedyIndSet(greedyIndSet,greedyIndSets)
import LevGraphs(levIntGraph)
import Graph(arrayGraph)
import System.Environment(getArgs)
import StrictLists(histogram)

main = getArgs >>= f

f :: [String] -> IO()
f [a,s,n] = alg a . arrayGraph $ levIntGraph (read s) (read n)
f _ = error "Need 3 args."

alg "one"   = print . length . greedyIndSet
alg "all" = print . histogram . map length . greedyIndSets
alg _ = error "options are 'one' and 'all'"