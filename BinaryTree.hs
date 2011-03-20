module BinaryTree where

import Bit
import Data.List(foldl')


data BinaryTree a = Leaf | 
                    Branch (Maybe a) (BinaryTree a) (BinaryTree a)

instance Show a => Show (BinaryTree a) where
    show Leaf = "_"
    show (Branch Nothing l r) = "(_," ++ show l ++ "," ++ show r ++ ")"
    show (Branch (Just x) l r) = "(" ++ show x ++ "," ++ show l ++ "," ++ show r ++ ")"

newNode x = Branch (Just x) Leaf Leaf


value Leaf = Nothing

value (Branch x l r) = x


getNode Leaf _ = Leaf

getNode t@(Branch x l r) [] = t

getNode (Branch x l r) (b:bs) = 
    if b
    then getNode r bs
    else getNode l bs


-- Ignores all leading zeros and one leading one, then calls f.
-- If there is no one, returns Nothing
ignorePrefixBits f [] = Nothing

ignorePrefixBits f (b:bs) = 
    if b
    then f bs
    else ignorePrefixBits f bs

stripPrefixBits [] = []

stripPrefixBits (b:bs) = 
    if b
    then bs
    else stripPrefixBits bs


safeGetValue tree = ignorePrefixBits (value . getNode tree)


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

listToTree xs = snd $ foldl' append ([], Leaf) xs

append (n, tree) x =
    (incrementNat n, setNode x tree n)