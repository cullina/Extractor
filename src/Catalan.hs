module Catalan where

import Util
import Data.List(foldl')

data Tree = Node [Tree]
          deriving Show

data BinaryTree = Leaf 
                | BNode BinaryTree BinaryTree
                deriving Show

data Paren = L 
           | R 
           deriving Show

treeToBinaryTree (Node [])     = Leaf
treeToBinaryTree (Node (t:ts)) = BNode (treeToBinaryTree t) (treeToBinaryTree (Node ts))

binaryTreeToTree = Node . bTTT
  where bTTT Leaf        = []
        bTTT (BNode l r) = Node (bTTT l) : bTTT r
        
treeToParens (Node ts) = concatMap (bracket . treeToParens) ts

parensToBinaryTree :: [Paren] -> Maybe BinaryTree
parensToBinaryTree ps = needNil =<< pTBT ps
  where pTBT :: [Paren] -> Maybe (BinaryTree, Maybe [Paren])
        pTBT []     = Just (Leaf, Nothing)
        pTBT (L:ps) = needR =<< pTBT ps
        pTBT (R:ps) = Just (Leaf, Just ps)
        
        needR (t, Nothing)   = Nothing
        needR (t, Just ps)   = fmap (mapFst (BNode t)) (pTBT ps)
                
        needNil :: (BinaryTree, Maybe [Paren]) -> Maybe BinaryTree
        needNil (t, Nothing) = Just t
        needNil _            = Nothing
        
         

          

bracket xs = [L] ++ xs ++ [R]
     
checkMatching = (Just 0 ==) . foldl' (>>=) (Just 0) . map toInt
  where toInt L = Just . succ
        toInt R = maybePred

allParens n = aP n 0
  where aP 0 0 = [[]]
        aP n m = l n m ++ r n m
        l  n m = if n > 0 then map (L :) (aP (n-1) (m+1)) else []
        r  n m = if m > 0 then map (R :) (aP  n    (m-1)) else []
        
        
stackSort

stackOp (x:xs, ys) L = Just (xs, x:ys, Nothing)
stackOp ([],    _) L = Nothing
stackOp (xs, y:ys) R = Just (xs, ys, Just y)
stackOp (_,    []) R = Nothing