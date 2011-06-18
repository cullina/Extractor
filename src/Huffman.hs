module Huffman where

import Data.Map(Map, fromList)
import Queue

-- |A| total leaves in tree
-- |B| leaves per vertex
-- v vertices
-- requires that |A| = 1+ v (|B| - 1)

data HuffmanTree b a = Branch Int (Map b (HuffmanTree b a)) | Leaf Int a | Empty

freq (Branch f m) = f
freq (Leaf   f x) = f

addFiller keys [] = []

addFiller [] leaves = []

addFiller (k:[]) leaves = []

addFiller (k:ks) leaves@((Leaf f x):ls) = 
    let filler = rem (length ls) (length ks)
    in replicate filler (Leaf 0 x) ++ leaves



takeMin ([], q) = 
    case deq q of 
        Nothing      -> (Nothing, ([], q))
        Just (y, q') -> (Just y, ([], q'))

takeMin ((x:xs), q) =
    case peek q of 
        Nothing           -> (Just x, (xs, q))
        Just (y, q', yq') -> if freq x <= freq y
                             then (Just x, (xs, yq'))
                             else (Just y, ((x:xs), q'))

bothEmpty ([], q) = Queue.empty q

bothEmpty _ = False
                                 
zipWithMin [] unused _ = ([], unused)

zipWithMin (k:ks) unused pairs =
    case takeMin unused of 
        (Just x,  unused') -> zipWithMin ks unused' ((k, x) : pairs)
        (Nothing, unused') -> ([], unused)


    
buildCode keys [] = Empty
    
buildCode keys leaves =
    let leaves' = addFiller keys leaves
        unused = (leaves', newQueue)
    in buildTree keys unused Empty
    

buildTree keys unused tree
    | bothEmpty unused = tree
    | otherwise        = let (pairs, unused') = zipWithMin keys unused []
                             f                = sum $ map (freq . snd) pairs
                             tree'            = Branch f (fromList pairs)
                         in buildTree keys unused' tree'
    

    