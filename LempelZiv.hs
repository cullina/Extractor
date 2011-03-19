module LempelZiv where

import BinaryTree
import Bitstream


data Chunk a = Chunk a Bool |
               TerminalChunk a
               deriving Show


parse :: Int -> BinaryTree Int -> Chunk Int -> [Bool] -> (BinaryTree Int, Chunk Int, [Bool])

parse n Leaf chunk bs = (newNode n, chunk, bs)

parse n t@(Branch (Just x) l r) _ [] = (t, TerminalChunk x, [])

parse n (Branch (Just x) l r) _ (b:bs) =
    let chunk = Chunk x b
    in if b
       then let (r', chunk', bs') = parse n r chunk bs
            in (Branch (Just x) l r', chunk', bs')
       else let (l', chunk', bs') = parse n l chunk bs
            in (Branch (Just x) l' r, chunk', bs')


completeParse bs = completeParse' 1 (newNode 0) bs

completeParse' n tree [] = []

completeParse' n tree bs = 
    let (tree', chunk, bs') = parse n tree (TerminalChunk 0) bs
    in case chunk of
         Chunk _ _       -> chunk:(completeParse' (n+1) tree' bs')
         TerminalChunk _ -> chunk:[]

bitsNeeded = bitsNeeded' 1 1 0

bitsNeeded' n m logM = 
    logM : if n < m
           then bitsNeeded' (n+1) m logM
           else bitsNeeded' (n+1) (2*m) (logM+1)                      


serialize cs = serialize' bitsNeeded cs

serialize' _ [] = []

serialize' (m:ms) ((TerminalChunk k):cs) =
    intWToBits m [] k

serialize' (m:ms) ((Chunk k b):cs) =
    intWToBits m [] k ++ b : serialize' ms cs
       

deserialize bs = deserialize' bitsNeeded bs

deserialize' (m:ms) bs =
    let (k, bs')    = splitAt m bs
    in case bs' of
         []       -> TerminalChunk k : []
         (b:bs'') -> Chunk k b : deserialize' ms bs''

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
