module UnbalancedTree where

import Bit

data UnbalancedTree = Leaf | Branch Int UnbalancedTree UnbalancedTree
                      deriving Show


sizeOf Leaf = 1

sizeOf (Branch size _ _) = size 

newBranch = Branch 2 Leaf Leaf




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
    
    
   

indexToBits Leaf index = (newBranch, [])

indexToBits (Branch size left right) index = 
       if index >= sizeOf left
       then let (right', bs) = indexToBits right (index - sizeOf left)
            in( Branch (size+1) left right', True : bs)
       else let (left', bs)  = indexToBits left index
            in( Branch (size+1) left' right, False : bs)

--does not change tree
internalIndexToBits _ 1 = []

internalIndexToBits (Branch size left right) index = 
   if index > sizeOf left
       then True : internalIndexToBits right (index - sizeOf left)
       else False : internalIndexToBits left (index - 1)


prefixCodeToInt max = prefixCodeToInt' max 1

prefixCodeToInt' max n (b:bs) =
    let n' = doubleIf n b
    in if n' >= max
       then (n' - max, Just bs)
       else prefixCodeToInt' max n' bs

prefixCodeToInt' max n [] = (n, Nothing)

-----------------------------------------------------------------------------------------------     

encode bs = concat $ encode' newBranch bs

encode' tree bs =
    case bitsToIndex tree 0 bs of
      Just (tree', index, bs') -> natToBits (index + sizeOf tree) : encode' tree' bs'
      Nothing                  -> natToBits (bitsToInternalIndex tree 1 bs) : []


decode bs = concat $ decode' newBranch bs

decode' tree bs =
    case prefixCodeToInt (sizeOf tree) bs of
      (index, Just bs') -> let (tree', bits) = indexToBits tree index
                           in bits : decode' tree' bs'
      (index, Nothing)  -> internalIndexToBits tree index : []
    
