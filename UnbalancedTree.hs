module UnbalancedTree where

import Bit

data UnbalancedTree = Leaf | Branch Int UnbalancedTree UnbalancedTree
                      deriving Show


sizeOf Leaf = 1

sizeOf (Branch size _ _) = size 

newBranch = Branch 2 Leaf Leaf


bitsToIndex Leaf bs = 
    (0, Just (newBranch, bs))

bitsToIndex (Branch _ _ _) [] = 
    (1, Nothing)

bitsToIndex (Branch size left right) (False:bs) =
    case bitsToIndex left bs of
      (index, Just (left', bs')) -> (index, Just (Branch (size+1) left' right, bs'))
      (index, Nothing)           -> (index + 1, Nothing)

bitsToIndex (Branch size left right) (True:bs) =
    case bitsToIndex right bs of
      (index, Just (right', bs')) -> (index + sizeOf left, Just (Branch (size+1) left right', bs'))
      (index, Nothing)            -> (index + sizeOf left, Nothing)
   

indexToBits Leaf index = ([], newBranch)

indexToBits (Branch size left right) index = 
       if index >= sizeOf left
       then let (bs, right') = indexToBits right (index - sizeOf left)
            in(True : bs, Branch (size+1) left right')
       else let (bs, left')  = indexToBits left index
            in(False : bs, Branch (size+1) left' right)

--does not change tree
internalIndexToBits _ 1 = []

internalIndexToBits (Branch size left right) index = 
   if index > sizeOf left
       then True : internalIndexToBits right (index - sizeOf left)
       else False : internalIndexToBits left (index - 1)

---------------------------------------------

prefixCodeToInt max n (b:bs) =
    let n' = doubleIf n b
    in if n' >= max
       then (n' - max, Just bs)
       else prefixCodeToInt max n' bs

prefixCodeToInt max n [] = (n, Nothing)

-----------------------------------------------------------

codeToIndices (m:ms) bs =
    case prefixCodeToInt m 1 bs of
      (index, Just bs') -> index : codeToIndices ms bs'
      (index, Nothing)  -> index : []
          

indicesToCode (m:ms) [] = []

indicesToCode (m:ms) (n:[]) = natToBits n : []

indicesToCode (m:ms) (n:ns) = natToBits (n + m) : indicesToCode ms ns

------------------------------------------------------

computeIndices tree bs = 
    case bitsToIndex tree bs of
      (index, Just (tree', bs')) -> index : computeIndices tree' bs'
      (index, Nothing)           -> index : []


translateIndices tree [] = []

translateIndices tree (n:[]) =
    internalIndexToBits tree n : []

translateIndices tree (n:ns) =
    let (bits, tree') = indexToBits tree n
    in bits : translateIndices tree' ns


-----------------------------------------------------------------------------------------------     

encode = concat . indicesToCode [2..] . computeIndices newBranch

decode = concat . translateIndices newBranch . codeToIndices [2..]
