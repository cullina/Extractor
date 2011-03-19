module BinaryTree where

import Bitstream


data BinaryTree a = Leaf | 
                    Branch (Maybe a) (BinaryTree a) (BinaryTree a)

instance Show a => Show (BinaryTree a) where
    show Leaf = "_"
    show (Branch Nothing l r) = "(_," ++ show l ++ "," ++ show r ++ ")"
    show (Branch (Just x) l r) = "(" ++ show x ++ "," ++ show l ++ "," ++ show r ++ ")"

newNode x = Branch (Just x) Leaf Leaf


getValue Leaf _ = Nothing

getValue (Branch x l r) [] = x

getValue (Branch x l r) (b:bs) = 
    if b
    then getValue r bs
    else getValue l bs


dereference tree [] = Nothing

dereference tree (b:bs) = 
    if b
    then getValue tree bs
    else dereference tree bs


modifyNode f Leaf [] = Branch (f Nothing) Leaf Leaf

modifyNode f (Branch x l r) [] = Branch (f x) l r

modifyNode f Leaf (b:bs) = 
    if b
    then Branch Nothing Leaf (modifyNode f Leaf bs)
    else Branch Nothing (modifyNode f Leaf bs) Leaf

modifyNode f (Branch x l r) (b:bs) =
    if b
    then Branch x l (modifyNode f r bs)
    else Branch x (modifyNode f l bs) r


setNode value = modifyNode (const (Just value))

listToTree cs = listToTree' 1 Leaf cs

listToTree' n tree [] = tree

listToTree' n tree (c:cs) = 
    let tree' = setNode c tree (natToBits [] n)
    in listToTree' (n+1) tree' cs

