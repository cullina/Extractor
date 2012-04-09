module QTree where

import Bit(showBits)

data BTree = BE 
           | BT BTree BTree 
           deriving Show

data TTree = TE 
           | TT TTree TTree TTree
           deriving Show

data QTree q =  QT (q -> Maybe (QTree q))

data GeneralTree = GenT [Maybe GeneralTree] deriving Show


tritLookup :: (a,a,a) -> Ordering -> a
tritLookup (_,_,x) LT = x
tritLookup (_,x,_) EQ = x
tritLookup (x,_,_) GT = x

showTrits = map f 
  where
    f LT = '0'
    f EQ = '1'
    f GT = '2'

bitLookup :: (a,a) -> Bool -> a
bitLookup (x,_) True  = x
bitLookup (_,x) False = x


qTreeToGeneral :: [q] -> QTree q -> GeneralTree
qTreeToGeneral qs (QT cs) = GenT $ map (fmap (qTreeToGeneral qs) . cs) qs



qString :: Eq q => [q] -> QTree q -> [q]
qString qs = f Nothing
  where
    f x (QT cs) = concatMap (g x cs) qs
    g x cs q = case (fmap (q ==) x, cs q) of
      (Just True,  Nothing) -> [q]
      (Just True,  Just t)  -> [q] ++ reverse (f Nothing t) ++ [q]
      (_,          Nothing) -> []
      (_,          Just t)  -> reverse (f (Just q) t)
        

bTreeToQTree :: BTree -> Maybe (QTree Bool)
bTreeToQTree BE       = Nothing
bTreeToQTree (BT l r) = Just . QT $ bitLookup (bTreeToQTree l, bTreeToQTree r)

tTreeToQTree :: TTree -> Maybe (QTree Ordering)
tTreeToQTree TE          = Nothing
tTreeToQTree (TT l m r) = Just . QT $ tritLookup (tTreeToQTree l, tTreeToQTree m, tTreeToQTree r)

bString :: BTree -> Maybe String
bString = fmap (showBits . qString [True, False]) . bTreeToQTree

tString :: TTree -> Maybe String
tString = fmap (showTrits . qString [GT, EQ, LT]) . tTreeToQTree


bString2 :: BTree -> Maybe String
bString2 BE = Nothing
bString2 (BT l r) = Just $ a l ++ b r
  where 
    a BE       = []              
    a (BT l r) = a l ++ b r ++ "1"
    b BE       = []              
    b (BT l r) = "0" ++ a l ++ b r
        