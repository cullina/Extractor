module Main where

import VT

main = mapM print $ concatMap countCliqueVTH' [2..10]
       