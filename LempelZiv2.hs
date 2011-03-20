module LempelZiv2 where

import BinaryTree
import Bit


data Chunk a = Chunk a Bool |
               TerminalChunk a
               deriving Show



parse n t chunk [] = 
    case t of
      Branch x Leaf           Leaf           -> (t, TerminalChunk x, [])
      Branch x (Branch _ _ _) (Branch _ _ _) -> (t, TerminalChunk x, [])
      _                                      -> (t, chunk, [])
    

parse n Leaf chunk bs = (newNode n, chunk, bs)

parse n (Branch x l r) _ (b:bs) =
    let chunk = case (l, r, b) of
                  (Branch _ _ _, Leaf, True)  -> TerminalChunk x
                  (Leaf, Branch _ _ _, False) -> TerminalChunk x
                  _                         -> Chunk x b
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
       

deserialize bs = deserialize' (append ([], Leaf) (False, False)) bitsNeeded bs

deserialize' t (m:ms) [] = []

deserialize' t (m:ms) bs =
    let (k, bs')   = splitAt m bs
        k'         = incrementInt k
        inferedBit = inferOtherChild $ safeGetValue (snd t) k'
        t'         = append t (False, False)
    in case (inferedBit, bs') of
         (Just iB, b:[])   -> Chunk k b : []
         (Just iB, _)      -> Chunk k iB : deserialize' (markChild iB t' k') ms bs'
         (Nothing, [])     -> TerminalChunk k : []
         (Nothing, b:bs'') -> Chunk k b : deserialize' (markChild b t' k') ms bs''



markChild b (n, tree) k 
    = (n, modifyNode (markChild' b) tree (stripPrefixBits k))

markChild' True  (Just (b, _)) = Just (b, True)

markChild' False (Just (_, b)) = Just (True, b)
    


inferOtherChild (Just (True, False)) = Just True

inferOtherChild (Just (False, True)) = Just False

inferOtherChild _ = Nothing



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
