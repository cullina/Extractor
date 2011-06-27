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

value (Branch x _ _) = x

left Leaf = Leaf

left (Branch _ l _) = l

right Leaf = Leaf

right (Branch _ _ r) = r

getNode Leaf _ = Leaf

getNode t@(Branch _ _ _) [] = t

getNode (Branch _ l r) (b:bs) = 
    if b
    then getNode r bs
    else getNode l bs


-- Ignores all leading zeros and one leading one, then calls f.
-- If there is no one, returns Nothing
ignorePrefixBits _ [] = Nothing

ignorePrefixBits f (b:bs) = 
    if b
    then f bs
    else ignorePrefixBits f bs


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


getDepth :: Int -> BinaryTree a -> [Maybe a]

getDepth n = concat . getDepth' n

getDepth' :: Int -> BinaryTree a -> [[Maybe a]]

getDepth' 0 _ = []

getDepth' n t =
    let l = getDepth' (n - 1) $ left t
        r = getDepth' (n - 1) $ right t
    in [value t] : zipWith (++) l r


findDepth Leaf = 0

findDepth (Branch _ l r) = 1 + max (findDepth l) (findDepth r)

flatten tree = getDepth (findDepth tree) tree 
