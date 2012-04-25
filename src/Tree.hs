{-# LANGUAGE FlexibleInstances #-}

module Tree where

import Data.Maybe(fromJust)
import Data.Foldable(foldrM)
import Control.Monad((<=<), join)
import Util(mapFst)
import RandomValue


data Tree m a = E
              | T a (m (Tree m a))
                
instance Functor m => Functor (Tree m) where
  fmap _ E        = E
  fmap f (T x cs) = T (f x) (fmap (fmap f) cs)

instance (Show a) => Show (Tree [] a) where
  show E = "_"
  show (T x cs) = show (x,cs)

instance (Show a) => Show (Tree Pair a) where
  show E = "_"
  show (T x (Pair l r)) = show (l,x,r)

data Pair a = Pair a a

data Triple a = Triple a a a 

instance Functor Pair where
  fmap f (Pair x y) = Pair (f x) (f y)
  
instance Functor Triple where
  fmap f (Triple x y z) = Triple (f x) (f y) (f z)
  
data LCode a = Open a
             | Close
               
instance Show a => Show (LCode a) where
  show Close = "X"
  show (Open x) = show x

-----------------------------

pairConstructor :: RValue a (Pair a)
pairConstructor = arr2 Pair

tripleConstructor :: RValue a (Triple a)
tripleConstructor = arr3 Triple

qListConstructor :: Int -> RValue a [a]
qListConstructor = useFixedNumber

------------------

bitLookup :: Pair a -> Bool -> a  
bitLookup (Pair x _) True  = x
bitLookup (Pair _ y) False = y

tritLookup :: Triple a -> Ordering -> a  
tritLookup (Triple x _ _) GT = x
tritLookup (Triple _ y _) EQ = y
tritLookup (Triple _ _ z) LT = z

bitCoerce :: [a] -> Pair a
bitCoerce [x,y] = Pair x y
bitCoerce _ = error "List is not length 2."

tritCoerce :: [a] -> Triple a
tritCoerce [x,y,z] = Triple x y z
tritCoerce _       = error "List is not length 3."


convertTree :: Functor m => (m (Tree n a) -> n (Tree n a)) -> Tree m a -> Tree n a
convertTree _  E       = E
convertTree f (T x cs) = T x $ f (fmap (convertTree f) cs)


bTreeToQTree :: Tree Pair a -> Tree ((->) Bool) a
bTreeToQTree = convertTree bitLookup

tTreeToQTree :: Tree Triple a -> Tree ((->) Ordering) a
tTreeToQTree = convertTree tritLookup

qTreeToGTree :: [q] -> Tree ((->) q) a -> Tree [] a
qTreeToGTree qs = convertTree (flip map qs)

gTreeToBTree :: Tree [] a -> Tree Pair a
gTreeToBTree = convertTree bitCoerce

gTreeToTTree :: Tree [] a -> Tree Triple a
gTreeToTTree = convertTree tritCoerce

-----------------------------------------------------------------------


qString :: Eq q => [q] -> Tree ((->) q) a -> Maybe [q]
qString qs E = Nothing
qString qs (T _ cs) = Just $ f Nothing cs
  where
    f x cs   = concatMap (g x cs) qs
    g x cs q = case (fmap (q ==) x, cs q) of
      (Just True,  E)      -> [q]
      (Just True,  T _ cs) -> [q] ++ reverse (f Nothing cs) ++ [q]
      (_,          E)      -> []
      (_,          T _ cs) -> reverse (f (Just q) cs)

qString2 :: Eq q => [q] -> Tree ((->) q) a -> Maybe [q]
qString2 qs E        = Nothing
qString2 qs (T _ cs) = Just $ f False Nothing cs
  where
    sq         = reverse qs
    f d x cs   = concatMap (g (not d) x cs) (if d then sq else qs)
    g d x cs q = case (fmap (q ==) x, cs q) of
      (Just True,  E)      -> [q]
      (Just True,  T _ cs) -> [q] ++ f d Nothing cs ++ [q]
      (_,          E)      -> []
      (_,          T _ cs) -> f d (Just q) cs
        

bString2 :: Tree Pair a -> Maybe [Bool]
bString2 E = Nothing
bString2 (T _ (Pair l r)) = Just $ a l ++ b r
  where 
    a E                = []              
    a (T _ (Pair l r)) = a l ++ b r ++ [True]
    b E                = []              
    b (T _ (Pair l r)) = [False] ++ a l ++ b r

----------------------------------------------------------------------

inorder :: Tree Pair a -> [a]
inorder E                = []
inorder (T x (Pair l r)) = inorder l ++ [x] ++ inorder r
                   
increasingBTree :: Ord a => [a] -> Tree Pair a
increasingBTree xs = f (E, xs) 
  where
    f :: Ord a => (Tree Pair a, [a]) -> Tree Pair a
    f (t, [])   = t
    f (t, x:xs) = f $ g t x (E, xs)
    
    g :: Ord a => Tree Pair a -> a -> (Tree Pair a, [a]) -> (Tree Pair a, [a])
    g left m (t, [])   = (T m (Pair left t), [])
    g left m (t, x:xs) = if x < m     
                         then (T m (Pair left t), x:xs)
                         else g left m (g t x (E, xs))
                        
increasingBTree2 :: Ord a => [a] -> Tree Pair a
increasingBTree2 = fromJust . fromLCode2 pairConstructor . permToLCode


-----------------------------------------------------------------------                              

permToLCode :: Ord a => [a] -> [LCode a]
permToLCode = fst . dump . foldr f ([Close], [])
  where 
    dump (xs, [])    = (xs, [])
    dump (xs, y:ys)  = dump (Open y : xs, ys)
    
    push y (xs, ys)  = (Close : xs, y : ys)
    
    f z s@(xs, [])   = push z s
    f z s@(xs, y:ys) = if z > y
                       then push z s
                       else push z (Open y : xs, ys)

toLCode :: (Ord q) => [q] -> Tree ((->) q) a -> [LCode a]
toLCode _   E       = [Close]
toLCode qs (T x cs) = Open x : concatMap (toLCode qs . cs) qs

fromLCode :: Int -> [LCode a] -> Maybe (Tree [] a)
fromLCode q = exactlyOne <=< foldrM f []
  where
    exactlyOne :: [b] -> Maybe b
    exactlyOne [t] = Just t
    exactlyOne _   = Nothing

    f Close    stack = Just (E : stack)
    f (Open x) stack = fmap (push x) (safeSplit q stack)

    push x (top, rest) = T x top : rest
    
    
safeSplit :: Int -> [t] -> Maybe ([t],[t])
safeSplit 0 ts     = Just ([], ts)
safeSplit n []     = Nothing
safeSplit n (t:ts) = fmap (mapFst (t :)) (safeSplit (n - 1) ts)

fromLCode2 :: RValue (Tree m a) (m (Tree m a)) -> [LCode a] -> Maybe (Tree m a)
fromLCode2 constructor = exactlyOne <=< foldrM f []
  where
    exactlyOne :: [b] -> Maybe b
    exactlyOne [t] = Just t
    exactlyOne _   = Nothing

    f Close    stack = Just (E : stack)
    f (Open x) stack = fmap (g x) (safeFromList constructor stack)
    
    g x (cs, stack) = T x cs : stack
    
