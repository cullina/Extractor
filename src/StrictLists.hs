module StrictLists where

data SP a b = SP !a !b deriving Show
toPair (SP x y) = (x, y)

fromPair (x, y) = SP x y

mapSSnd f (SP x y) = SP x (f y)

data SL a = SNil
          | SCons !a !(SL a)
          deriving Show            

data LL a = LNil
          | LCons !a (LL a)
            
fromSL :: SL a -> [a]            
fromSL SNil = []
fromSL (SCons x xs) = x : fromSL xs

fromLL :: LL a -> [a]            
fromLL LNil = []
fromLL (LCons x xs) = x : fromLL xs

fromList :: [a] -> LL a
fromList [] = LNil
fromList (x:xs) = LCons x (fromList xs)

mapS :: (a -> b) -> SL a -> SL b
mapS _ SNil = SNil
mapS f (SCons x xs) = SCons (f x) (mapS f xs)

mapL :: (a -> b) -> LL a -> LL b
mapL _ LNil = LNil
mapL f (LCons x xs) = LCons (f x) (mapL f xs)

toS :: (a,b) -> SP a (SL b)
toS (x,y) = SP x (SCons y SNil)

fromS :: SP a (SL b) -> (a, [b])
fromS (SP x y) = (x, fromSL y)

deleteS :: Eq a => a -> SL a -> SL a
deleteS _ SNil = SNil
deleteS x (SCons y ys) = 
  if x == y
  then ys
  else SCons y (deleteS x ys)

listSetFromList :: Ord a => [a] -> [a]
listSetFromList = fromSL . listSetFromListS . fromList

listSetFromListS :: Ord a => LL a -> SL a
listSetFromListS = mergeAll mergeSets . mapL (flip SCons SNil)

listMapFromList :: (Ord a, Ord b) => [(a,b)] -> [(a,[b])]
listMapFromList = map fromS . fromSL . listMapFromListS . fromList . map toS
    
listMapFromListS :: (Ord a, Ord b) => LL (SP a (SL b)) -> SL (SP a (SL b))
listMapFromListS = listMapUnion . mapL (flip SCons SNil)
    
listMapUnion :: (Ord a, Ord b) => LL (SL (SP a (SL b))) -> SL (SP a (SL b))
listMapUnion = mergeAll mergeMaps

                   
mergeAll :: (SL a -> SL a -> SL a) -> LL (SL a) -> SL a
mergeAll _     LNil           = SNil
mergeAll _     (LCons x LNil) = x
mergeAll merge xs             = mergeAll merge (mergePairs merge xs)
    
mergePairs :: (SL a -> SL a -> SL a) -> LL (SL a) -> LL (SL a)
mergePairs merge (LCons x (LCons y zs)) = LCons (merge x y) (mergePairs merge zs)
mergePairs _     zs                     = zs

mergeSets :: Ord a => SL a -> SL a -> SL a
mergeSets xx@(SCons x xs) yy@(SCons y ys) = 
  case compare x y of 
    EQ -> SCons x (mergeSets xs ys)
    LT -> SCons x (mergeSets xs yy)
    GT -> SCons y (mergeSets xx ys)
mergeSets xx SNil = xx
mergeSets SNil yy = yy

mergeMaps :: (Ord a, Ord b) => SL (SP a (SL b)) -> SL (SP a (SL b)) -> SL (SP a (SL b))
mergeMaps xx@(SCons (SP x fx) xs) yy@(SCons (SP y fy) ys) = 
  case compare x y of 
    EQ -> SCons (SP x (mergeSets fx fy)) (mergeMaps xs ys)
    LT -> SCons (SP x fx)              (mergeMaps xs yy)
    GT -> SCons (SP y fy)              (mergeMaps xx ys)
mergeMaps xx SNil = xx
mergeMaps SNil yy = yy

