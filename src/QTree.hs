module QTree where

import Bit(showBits)
import LevOps(vtSum)
import Permutation(allPermutations)

data BTree a = BE 
             | BT a (BTree a) (BTree a)
             deriving Show

data TTree a = TE 
             | TT a (TTree a) (TTree a) (TTree a)
             deriving Show

data QTree q a =  QT a (q -> Maybe (QTree q a))

instance Functor BTree where
  fmap _ BE = BE
  fmap f (BT x l r) = BT (f x) (fmap f l) (fmap f r)

instance Functor TTree where
  fmap _ TE = TE
  fmap f (TT x l m r) = TT (f x) (fmap f l) (fmap f m) (fmap f r)

instance Functor (QTree q) where
  fmap f (QT x cs) = QT (f x) (fmap (fmap (fmap f)) cs)

--data LQTree q = LQT (q -> Either (QTree q) Int)

data GeneralTree = GenT [Maybe GeneralTree] deriving Show


tritLookup :: (a,a,a) -> Ordering -> a
tritLookup (_,_,x) LT = x
tritLookup (_,x,_) EQ = x
tritLookup (x,_,_) GT = x

showTrits :: [Ordering] -> String
showTrits = map f 
  where
    f LT = '0'
    f EQ = '1'
    f GT = '2'

bitLookup :: (a,a) -> Bool -> a
bitLookup (x,_) True  = x
bitLookup (_,x) False = x


qTreeToGeneral :: [q] -> QTree q a -> GeneralTree
qTreeToGeneral qs (QT _ cs) = GenT $ map (fmap (qTreeToGeneral qs) . cs) qs



qString :: Eq q => [q] -> QTree q a -> [q]
qString qs = f Nothing
  where
    f x (QT _ cs) = concatMap (g x cs) qs
    g x cs q = case (fmap (q ==) x, cs q) of
      (Just True,  Nothing) -> [q]
      (Just True,  Just t)  -> [q] ++ reverse (f Nothing t) ++ [q]
      (_,          Nothing) -> []
      (_,          Just t)  -> reverse (f (Just q) t)
        

qString2 :: Eq q => [q] -> QTree q a -> [q]
qString2 qs = f False Nothing
  where
    sq = reverse qs
    f d x (QT _ cs) = concatMap (g d x cs) (if d then sq else qs)
    g d x cs q = case (fmap (q ==) x, cs q) of
      (Just True,  Nothing) -> [q]
      (Just True,  Just t)  -> [q] ++ f (not d) Nothing t ++ [q]
      (_,          Nothing) -> []
      (_,          Just t)  -> f (not d) (Just q) t
        

{-
qNumbering :: Eq q => [q] -> QTree q -> LQTree q
qNumbering qs = f 0 Nothing
  where
    f n 
-}


bTreeToQTree :: BTree a -> Maybe (QTree Bool a)
bTreeToQTree BE         = Nothing
bTreeToQTree (BT x l r) = Just . QT x $ bitLookup (bTreeToQTree l, bTreeToQTree r)

tTreeToQTree :: TTree a -> Maybe (QTree Ordering a)
tTreeToQTree TE           = Nothing
tTreeToQTree (TT x l m r) = Just . QT x $ tritLookup (tTreeToQTree l, tTreeToQTree m, tTreeToQTree r)

bString :: BTree a -> Maybe [Bool]
bString = fmap (qString [True, False]) . bTreeToQTree

tString :: TTree a -> Maybe String
tString = fmap (showTrits . qString [GT, EQ, LT]) . tTreeToQTree


bString2 :: BTree a -> Maybe [Bool]
bString2 BE = Nothing
bString2 (BT _ l r) = Just $ a l ++ b r
  where 
    a BE         = []              
    a (BT _ l r) = a l ++ b r ++ [True]
    b BE         = []              
    b (BT _ l r) = [False] ++ a l ++ b r
        
inorder :: BTree a -> [a]
inorder BE         = []
inorder (BT x l r) = inorder l ++ [x] ++ inorder r
                   
increasingBTree :: Ord a => [a] -> BTree a
increasingBTree xs = f (BE, xs) 
  where
    f :: Ord a => (BTree a, [a]) -> BTree a
    f (t, [])   = t
    f (t, x:xs) = f $ g t x (BE, xs)
    
    g :: Ord a => BTree a -> a -> (BTree a, [a]) -> (BTree a, [a])
    g left m (t, [])   = (BT m left t, [])
    g left m (t, x:xs) = if x < m     
                         then (BT m left t, x:xs)
                         else g left m (g t x (BE, xs))
