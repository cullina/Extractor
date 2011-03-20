module Histogram 
    ( 
     histogram,
     treeHistogram,
     flatten
    ) where

import Data.List(foldl', transpose)


data TreeHistogram = EmptyH |
                     HNode Int TreeHistogram TreeHistogram

instance Show TreeHistogram where
    show tree = show $ flatten tree


--histogram

histogramUpdate [] 0 = [1]

histogramUpdate [] n = 0 : histogramUpdate [] (n - 1)

histogramUpdate (h:hs) 0 = (h + 1) : hs

histogramUpdate (h:hs) n = h : histogramUpdate hs (n - 1)


histogram :: [Int] -> [Int]

histogram = foldl' histogramUpdate []

{----------------------------------------------------------------}

newNode = HNode 0 EmptyH EmptyH

incNode (HNode count left right) = HNode (count + 1) left right

branchNode (HNode count left right) n =
    let (q, r) = quotRem n 2
    in if r == 0
       then HNode count (treeHistogramUpdate left q) right
       else HNode count left (treeHistogramUpdate right q)


treeHistogramUpdate EmptyH 1 = incNode newNode

treeHistogramUpdate node@(HNode _ _ _) 1 = incNode node

treeHistogramUpdate EmptyH n = branchNode newNode n

treeHistogramUpdate node@(HNode _ _ _) n = branchNode node n

treeHistogram = flatten . foldl' treeHistogramUpdate EmptyH . map (+ 1)


getNode n tree = getNode' (n + 1) tree

getNode' _ EmptyH = 0

getNode' 1 (HNode count _ _) = count

getNode' n (HNode count left right) =
    let (q, r) = quotRem n 2
    in if r == 0
       then getNode' q left
       else getNode' q right

getDepth 1 (HNode count _ _) = [count]

getDepth n EmptyH = replicate (2 ^ (n - 1)) 0

getDepth n (HNode count left right) =
    let l = getDepth (n - 1) left 
        r = getDepth (n - 1) right
    in count : concat (transpose [l,r])


findDepth EmptyH = 0

findDepth (HNode count left right) = 1 + max (findDepth left) (findDepth right)

flatten tree = getDepth (findDepth tree) tree 