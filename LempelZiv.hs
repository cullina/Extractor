module LempelZiv where

import BinaryTree
import Bit


data Chunk a = Chunk a Bool |
               TerminalChunk a
               deriving Show



parse n Leaf chunk bs = (newNode n, chunk, bs)

parse n t@(Branch x l r) _ [] = (t, TerminalChunk x, [])

parse n (Branch x l r) _ (b:bs) =
    let chunk = Chunk x b
    in if b
       then let (r', chunk', bs') = parse n r chunk bs
            in (Branch x l r', chunk', bs')
       else let (l', chunk', bs') = parse n l chunk bs
            in (Branch x l' r, chunk', bs')


completeParse bs = completeParse' 1 (newNode 0) bs

completeParse' n tree [] = []

completeParse' n tree bs = 
    let (tree', chunk, bs') = parse n tree (TerminalChunk Nothing) bs
    in chunk:(completeParse' (n+1) tree' bs')


bitsNeeded = bitsNeeded' 1 1 0

bitsNeeded' n m logM = 
    logM : if n < m
           then bitsNeeded' (n+1) m logM
           else bitsNeeded' (n+1) (2*m) (logM+1)                      


serialize cs = serialize' bitsNeeded cs

serialize' _ [] = []

serialize' (m:ms) ((TerminalChunk (Just k)):cs) =
    intWToBits m [] k ++ serialize' ms cs

serialize' (m:ms) ((Chunk (Just k) b):cs) =
    intWToBits m [] k ++ b : serialize' ms cs
       

deserialize bs = deserialize' bitsNeeded bs

deserialize' (m:ms) bs =
    let (k, bs')    = splitAt m bs
    in case bs' of
         []       -> TerminalChunk k : []
         (b:bs'') -> Chunk k b : deserialize' ms bs''

translate tree bs Nothing = bs

translate tree bs (Just (TerminalChunk k)) = 
    translate tree bs $ safeGetValue tree k

translate tree bs (Just (Chunk k b)) = 
    translate tree (b:bs) $ safeGetValue tree k


encodeLempelZiv = serialize . completeParse
    

decodeLempelZiv bits = 
    let chunks = deserialize bits
        tree   = listToTree chunks
    in concatMap (translate tree [] . Just) chunks
