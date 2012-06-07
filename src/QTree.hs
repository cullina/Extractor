module QTree where

import Bit(showBits)
import LevOps(vtSum)
import Permutation(allPermutations)
import Data.Foldable(foldrM)
import Control.Monad((<=<), join)
import Data.Maybe(fromJust)
import Util(mapFst)

data BTree a = BE 
             | BT a (BTree a) (BTree a)
             deriving Show

data TTree a = TE 
             | TT a (TTree a) (TTree a) (TTree a)
             deriving Show

data QTree q a = QE 
               | QT a (q -> QTree q a)

data GeneralTree a = GenE
                   | GenT a [GeneralTree a]
                   deriving Show

instance Functor BTree where
  fmap _ BE         = BE
  fmap f (BT x l r) = BT (f x) (fmap f l) (fmap f r)

instance Functor TTree where
  fmap _ TE           = TE
  fmap f (TT x l m r) = TT (f x) (fmap f l) (fmap f m) (fmap f r)

instance Functor (QTree q) where
  fmap _ QE        = QE
  fmap f (QT x cs) = QT (f x) (fmap f . cs)

instance Functor GeneralTree where
  fmap f GenE        = GenE
  fmap f (GenT x cs) = GenT (f x) (map (fmap f) cs)


data LCode a = Open a
             | Close
               
instance Show a => Show (LCode a) where
  show Close = "X"
  show (Open x) = show x

bitCoerce :: [a] -> (a,a)
bitCoerce [x,y] = (x,y)
bitCoerce _     = error "List is not length 2."

tritCoerce :: [a] -> (a,a,a)
tritCoerce [x,y,z] = (x,y,z)
tritCoerce _       = error "List is not length 3."

bitLookup :: (a,a) -> Bool -> a
bitLookup (x,_) True  = x
bitLookup (_,x) False = x

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


qTreeToGeneral :: [q] -> QTree q a -> GeneralTree a
qTreeToGeneral _  QE        = GenE
qTreeToGeneral qs (QT x cs) = GenT x $ map (qTreeToGeneral qs . cs) qs

genTreeToQ :: GeneralTree a -> QTree Int a
genTreeToQ GenE        = QE
genTreeToQ (GenT x ts) = QT x (\q -> map genTreeToQ ts !! q)


qString :: Eq q => [q] -> QTree q a -> Maybe [q]
qString qs QE = Nothing
qString qs (QT _ cs) = Just $ f Nothing cs
  where
    f x cs   = concatMap (g x cs) qs
    g x cs q = case (fmap (q ==) x, cs q) of
      (Just True,  QE)      -> [q]
      (Just True,  QT _ cs) -> [q] ++ reverse (f Nothing cs) ++ [q]
      (_,          QE)      -> []
      (_,          QT _ cs) -> reverse (f (Just q) cs)
        

qString2 :: Eq q => [q] -> QTree q a -> Maybe [q]
qString2 qs QE        = Nothing
qString2 qs (QT _ cs) = Just $ f False Nothing cs
  where
    sq         = reverse qs
    f d x cs   = concatMap (g d x cs) (if d then sq else qs)
    g d x cs q = case (fmap (q ==) x, cs q) of
      (Just True,  QE)      -> [q]
      (Just True,  QT _ cs) -> [q] ++ f (not d) Nothing cs ++ [q]
      (_,          QE)      -> []
      (_,          QT _ cs) -> f (not d) (Just q) cs
        

bTreeToQTree :: BTree a -> QTree Bool a
bTreeToQTree BE         = QE
bTreeToQTree (BT x l r) = QT x $ bitLookup (bTreeToQTree l, bTreeToQTree r)

tTreeToQTree :: TTree a -> QTree Ordering a
tTreeToQTree TE           = QE
tTreeToQTree (TT x l m r) = QT x $ tritLookup (tTreeToQTree l, tTreeToQTree m, tTreeToQTree r)

bString :: BTree a -> Maybe [Bool]
bString = qString [True, False] . bTreeToQTree

tString :: TTree a -> Maybe String
tString = fmap showTrits . qString [GT, EQ, LT] . tTreeToQTree


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



toLCode :: (Ord q) => [q] -> QTree q a -> [LCode a]
toLCode _   QE       = [Close]
toLCode qs (QT x cs) = Open x : concatMap (toLCode qs . cs) qs


fromLCode :: Eq q => [q] -> [LCode a] -> Maybe (QTree q a)
fromLCode qs = exactlyOne <=< foldrM f []
  where
    exactlyOne :: [b] -> Maybe b
    exactlyOne [t] = Just t
    exactlyOne _   = Nothing

    f Close    stack = Just (QE : stack)
    f (Open x) stack = fmap (push x) (collectChildren qs stack)

    push :: Eq q => a -> ([(q, QTree q a)], [QTree q a]) -> [QTree q a]
    push x (top, rest) = QT x (fromJust . flip lookup top) : rest

fromLCode2 :: Int -> [LCode a] -> Maybe (GeneralTree a)
fromLCode2 q = exactlyOne <=< foldrM f []
  where
    exactlyOne :: [b] -> Maybe b
    exactlyOne [t] = Just t
    exactlyOne _   = Nothing

    f Close    stack = Just (GenE : stack)
    f (Open x) stack = fmap (push x) (safeSplit q stack)

    push x (top, rest) = GenT x top : rest

    
safeSplit :: Int -> [t] -> Maybe ([t],[t])
safeSplit 0 ts     = Just ([], ts)
safeSplit n []     = Nothing
safeSplit n (t:ts) = fmap (mapFst (t :)) (safeSplit (n - 1) ts)
    
collectChildren :: [q] -> [t] -> Maybe ([(q,t)],[t])
collectChildren []     ts     = Just ([], ts)
collectChildren (q:qs) []     = Nothing
collectChildren (q:qs) (t:ts) = fmap (mapFst ((q,t) :)) (collectChildren qs ts)