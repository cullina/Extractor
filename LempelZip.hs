module LempelZiv where

import BinaryTree
import Bitstream


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
    let (tree', chunk, bs') = parse n tree (TerminalChunk 0) bs
    in case chunk of
         Chunk _ _       -> chunk:(completeParse' (n+1) tree' bs')
         TerminalChunk _ -> chunk:[]


serialize cs = serialize' 1 1 0 cs

serialize' _ _ _ [] = []

serialize' n m logM ((TerminalChunk k):cs) =
    intWToBits logM [] k

serialize' n m logM ((Chunk k b):cs) =
    let (m', logM') = if n < m
                      then (m, logM)
                      else (2*m, logM+1)                      
    in intWToBits logM [] k ++ b : serialize' (n+1) m' logM' cs
       

deserialize bs = deserialize' 1 1 0 bs

deserialize' n m logM bs =
    let (k, bs')    = splitAt logM bs
        (m', logM') = if n < m
                      then (m, logM)
                      else (2*m, logM+1)                      
    in case bs' of
         []       -> TerminalChunk k : []
         (b:bs'') -> Chunk k b : deserialize' (n+1) m' logM' bs''

translate tree bs Nothing = bs

translate tree bs (Just (TerminalChunk k)) = 
    translate tree bs $ dereference tree k

translate tree bs (Just (Chunk k b)) = 
    translate tree (b:bs) $ dereference tree k


encodeLempelZiv = serialize . completeParse
    

decodeLempelZiv bits = 
    let chunks = deserialize bits
        tree   = listToTree chunks
    in concatMap (translate tree [] . Just) chunks
                       