module Ten where

import SubsetSelection(getSubset)
import Histogram
import QaryGraphs

unQ :: Qary -> Int
unQ (Qary n) = n

updown :: [Int] -> [Bool]
updown (x : y : zs) = (y >= x) : updown (y:zs)
updown _ = []

f :: Int -> [Int] -> Int
f n xs = rem (sum . getSubset [1..] $ updown xs) n

g :: Int -> [Int] -> Int
g q xs = rem (sum xs) q

h q n xs = q * f n xs + g q xs

test q n = histogram . map (h q n . map unQ) $ allQStrings q n 

maxtest q = maximum . test q