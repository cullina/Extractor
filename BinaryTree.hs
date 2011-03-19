module BinaryTree where

import Bitstream


data BinaryTree a = Leaf | 
                    Branch a (BinaryTree a) (BinaryTree a)

instance Show a => Show (BinaryTree a) where
    show Leaf = "_"
    show (Branch x l r) = "(" ++ show x ++ "," ++ show l ++ "," ++ show r ++ ")"

newNode x = Branch x Leaf Leaf


getValue Leaf _ = Nothing

getValue (Branch x l r) [] = Just x

getValue (Branch x l r) (b:bs) = 
    if b
    then getValue r bs
    else getValue l bs


dereference tree [] = Nothing

dereference tree (b:bs) = 
    if b
    then getValue tree bs
    else dereference tree bs


modifyNode defVal f Leaf [] = newNode (f defVal)

modifyNode defVal f (Branch x l r) [] = Branch (f x) l r

modifyNode defVal f Leaf (b:bs) = 
    if b
    then Branch defVal Leaf (modifyNode defVal f Leaf bs)
    else Branch defVal (modifyNode defVal f Leaf bs) Leaf

modifyNode defVal f (Branch x l r) (b:bs) =
    if b
    then Branch x l (modifyNode defVal f r bs)
    else Branch x (modifyNode defVal f l bs) r


setNode defVal value = modifyNode defVal (const value)

listToTree cs = listToTree' 1 Leaf cs

listToTree' n tree [] = tree

listToTree' n tree (c:cs) = 
    let tree' = setNode c c tree (natToBits [] n)
    in listToTree' (n+1) tree' cs

