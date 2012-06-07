module UnbalancedTree where

import Bit

data UnbalancedTree = Leaf | Branch Int UnbalancedTree UnbalancedTree
                      deriving Show


sizeOf Leaf = 1

sizeOf (Branch size _ _) = size 


newSubTree 0 = Leaf

newSubTree depth = 
    let t = newSubTree (depth - 1)
    in Branch (2 * sizeOf t) t t


unbalancedSubTree = unbalancedSubTree' . natToBits

unbalancedSubTree' [] = Leaf

unbalancedSubTree' (True:bs) = 
    let l = newSubTree (1 + length bs)
        r = unbalancedSubTree' bs
    in Branch (sizeOf l + sizeOf r) l r

unbalancedSubTree' (False:bs) = 
    let l = unbalancedSubTree' bs
        r = newSubTree (length bs)
    in Branch (sizeOf l + sizeOf r) l r




bitsToIndex subTree Leaf bs = 
    (0, Just (subTree, bs))

bitsToIndex _subTree (Branch _ _ _) [] = 
    (1, Nothing)

bitsToIndex subTree (Branch size left right) (False:bs) =
    case bitsToIndex subTree left bs of
      (index, Just (left', bs')) -> (index, Just (Branch (size+1) left' right, bs'))
      (index, Nothing)           -> (index + 1, Nothing)

bitsToIndex subTree (Branch size left right) (True:bs) =
    case bitsToIndex subTree right bs of
      (index, Just (right', bs')) -> (index + sizeOf left, Just (Branch (size+1) left right', bs'))
      (index, Nothing)            -> (index + sizeOf left, Nothing)
   

indexToBits subTree  Leaf _index = ([], subTree)

indexToBits subTree (Branch size left right) index = 
       if index >= sizeOf left
       then let (bs, right') = indexToBits subTree right (index - sizeOf left)
            in(True : bs, Branch (size+1) left right')
       else let (bs, left')  = indexToBits subTree left index
            in(False : bs, Branch (size+1) left' right)

--does not change tree
internalIndexToBits _ 1 = []

internalIndexToBits (Branch _ left right) index = 
   if index > sizeOf left
       then True : internalIndexToBits right (index - sizeOf left)
       else False : internalIndexToBits left (index - 1)

internalIndexToBits Leaf _index = error "index is not internal to tree, it is too large."

---------------------------------------------

-- second argument should initially be 1
-- returns either  
-- (x, Just bs)     x in [0, max)
-- (y, Nothing)     y in [1, max)

prefixCodeToInt 1 bs = (0, Just bs)

prefixCodeToInt max bs = pCTI max 1 bs where
    pCTI max n (b:bs) =
        let n' = doubleIf n b
        in if n' >= max
           then (n' - max, Just bs)
           else pCTI max n' bs

    pCTI _ n [] = (n, Nothing)

-----------------------------------------------------------

-- second argument should initially be 0
-- returns either  
-- (x, Just bs)     x in [0, (base - 1) * max]
-- (y, Nothing)     y in [0, max)


nonbinaryPrefixCodeToInt _base 0 _ bs = (0, Just bs)

nonbinaryPrefixCodeToInt base max n (x:xs) = 
    let n' = base * n + x
    in if n' >= max
       then (n' - max, Just xs)
       else nonbinaryPrefixCodeToInt base max n' xs

nonbinaryPrefixCodeToInt _base _max n [] = (n, Nothing)


-----------------------------------------------------------

codeToIndices incr max bs =
    case prefixCodeToInt max bs of
      (index, Just bs') -> index : codeToIndices incr (max + incr) bs'
      (index, Nothing)  -> [index]
          

indicesToCode _incr _max []     = []
indicesToCode _incr _max (n:[]) = [natToBits n]
indicesToCode incr  max  (n:ns) = natToBits (n + max) : indicesToCode incr (max + incr) ns

------------------------------------------------------

computeIndices subTree tree bs = 
    case bitsToIndex subTree tree bs of
      (index, Just (tree', bs')) -> index : computeIndices subTree tree' bs'
      (index, Nothing)           -> [index]


translateIndices _subTree _tree []     = []
translateIndices _subTree tree  (n:[]) = [internalIndexToBits tree n]
translateIndices subTree  tree  (n:ns) =
    let (bits, tree') = indexToBits subTree tree n
    in bits : translateIndices subTree tree' ns


-----------------------------------------------------------------------------------------------     

encode subTree = concat . indicesToCode (sizeOf subTree - 1) 1 . computeIndices subTree Leaf

decode subTree = concat . translateIndices subTree Leaf . codeToIndices (sizeOf subTree - 1) 1

depthEncode depth = encode (newSubTree depth)

depthDecode depth = decode (newSubTree depth)

{-
natEncode max = codeToIndices 0 max . 
                encode (unbalancedSubTree max) . 
                concatMap (natToBits . (+) max)

natDecode max = codeToIndices 0 max . 
                decode (unbalancedSubTree max) . 
                concatMap (natToBits . (+) max)
-}


