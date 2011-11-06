module Main where

import VT

main = mapM (print . countCliqueVT) [2..6]
       