module UnbalancedTree where

import Bit

data UnbalancedTree = Leaf | 
                      Branch Int UnbalancedTree UnbalancedTree


sizeOf Leaf = 1

sizeOf (Branch size _ _) = size 

newBranch = Branch 2 Leaf Leaf


bitsToIndexList bs = bitsToIndexList' [True, False] newBranch bs

bitsToIndexList' max tree bs =
    case bitsToIndex tree 0 bs of
      Just (tree', index, bs') -> toPrefixCode max index : bitsToIndexList' (incrementInt max) tree' bs'
      Nothing                  -> natToBits (bitsToInternalIndex tree 0 bs) : []



bitsToIndex Leaf index bs =
    Just (newBranch, index, bs)

bitsToIndex (Branch _ _ _) _ [] = Nothing

bitsToIndex (Branch size left right) index (False:bs) =
    case bitsToIndex left index bs of
      Just (left', index', bs') -> Just (Branch (size+1) left' right, index', bs')
      Nothing                   -> Nothing

bitsToIndex (Branch size left right) index (True:bs) =
    case bitsToIndex right (index + sizeOf left) bs of
      Just (right', index', bs') -> Just (Branch (size+1) left right', index', bs')
      Nothing                    -> Nothing

--does not change tree
bitsToInternalIndex tree index [] = index

bitsToInternalIndex (Branch size left right) index (False:bs) =
    bitsToInternalIndex left (index + 1) bs

bitsToInternalIndex (Branch size left right) index (True:bs) =
    bitsToInternalIndex right (index + sizeOf left) bs
    
    


    

indexToBits Leaf index = 
    (newBranch, [])

indexToBits (Branch size left right) index = 
       if index >= sizeOf left
       then let (right', bs) = indexToBits right (index - sizeOf left)
            in( Branch (size+1) left right', True : bs)
       else let (left', bs)  = indexToBits left index
            in( Branch (size+1) left' right, False : bs)

--does not change tree
internalIndexToBits _ 0 = []

internalIndexToBits (Branch size left right) index = 
   if index >= sizeOf left
       then True : internalIndexToBits right (index - sizeOf left)
       else False : internalIndexToBits left (index - 1)


     