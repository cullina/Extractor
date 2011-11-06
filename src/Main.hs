module Main where

import VT

main = mapM (print . countCliques (levEdges 1 5) 32) [2..6]
       