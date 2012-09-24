module Main where

import GreedyIndSet
import LevGraphs(levIntGraph)
import Graph(arrayGraph)
import System.Environment(getArgs)
import StrictLists(histogram)

main = getArgs >>= f

f :: [String] -> IO()
f [a,s,n]   = alg a . arrayGraph $ levIntGraph (read s) (read n)
f [a,s,n,m] = algm a (read m) . arrayGraph $ levIntGraph (read s) (read n)
f _         = error "Need 3 or 4 args."

alg "one" = print . length . greedyIndSet
alg "all" = print . histogram . map length . greedyIndSets
alg _ = error "options are 'one' and 'all'"

algm "one" m = print . length . fst . greedyIndSetN m 
algm "all" m = print . histogram . map length . greedyIndSetFirstN m
algm _     _ = error "options are 'one' and 'all'"