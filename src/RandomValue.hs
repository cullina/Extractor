module RandomValue where

--import Util
import Control.Monad(replicateM, ap)
import Control.Applicative

data RValue a b = Done b 
                | NotDone (a -> RValue a b)

instance Functor (RValue a) where
    fmap f (Done x)    = Done (f x)
    fmap f (NotDone g) = NotDone $ \a -> fmap f (g a)

instance Applicative (RValue a) where
    pure = return
    (<*>) = ap

instance Monad (RValue a) where 
    return = Done
    
    (Done x)    >>= g = g x
    (NotDone f) >>= g = NotDone $ \a -> f a >>= g
    
instance (Show b) => Show (RValue a b) where
  show (Done x) = "Done " ++ show x
  show (NotDone _) = "NotDone"
    

preMap _ (Done x)    = Done x
preMap f (NotDone g) = NotDone $ (\a -> preMap f (g a)) . f

composeRValues :: RValue a b -> RValue b c -> RValue a c

composeRValues _ (Done x) = Done x
  
composeRValues ry (NotDone f) = 
  (composeRValues ry . f) =<< ry
  


fmapD :: (Functor f) => f (a -> b) -> a -> f b

fmapD r x = fmap ($ x) r 

arr :: (a -> b) -> RValue a b 
arr f = 
  NotDone $ \x -> 
  Done (f x)

arr2 :: (a -> a -> b) -> RValue a b
arr2 f = 
  NotDone $ \x -> 
  NotDone $ \y -> 
  Done (f x y)

arr3 :: (a -> a -> a -> b) -> RValue a b
arr3 f = 
  NotDone $ \x -> 
  NotDone $ \y -> 
  NotDone $ \z -> 
  Done (f x y z)

untilSuccess :: RValue a (Maybe b) -> RValue a b

untilSuccess f = maybe (untilSuccess f) Done =<< f 


useFixedNumber n = replicateM n $ arr id

pP = mapM (const (arr id))

fromList :: RValue a b -> [a] -> (b, [a])
fromList (Done x)    as     = (x, as)
fromList (NotDone f) (a:as) = fromList (f a) as
fromList (NotDone _) []     = error "Reached end of list."

safeFromList :: RValue a b -> [a] -> Maybe (b, [a])
safeFromList (Done x)    as     = Just (x, as)
safeFromList (NotDone f) (a:as) = safeFromList (f a) as
safeFromList (NotDone _) []     = Nothing

attachCounter = aC 0
    where aC counter (Done x)    = Done (x, counter)
          aC counter (NotDone f) = NotDone $ \a -> aC (counter + 1) (f a)
          

useInput (Done x) = const (Done x)
useInput (NotDone f) = f

toMaybeD :: RValue a b -> Maybe b
toMaybeD (Done x)    = Just x
toMaybeD (NotDone _) = Nothing

toMaybeND :: RValue a b -> Maybe (a -> RValue a b)
toMaybeND (Done _)    = Nothing
toMaybeND (NotDone f) = Just f

allDone = sequence . map toMaybeD

{-
untilAllDone :: ([RValue a b] -> RValue a [RValue a b]) -> [RValue a b] -> RValue a [b]
untilAllDone f rs = 
  case allDone rs of
    Just xs -> Done xs
    Nothing -> untilAllDone f =<< f rs

parallel :: [RValue a b] -> RValue a [b]
parallel = untilAllDone parallelPass
    
parallelPass :: [RValue a b] -> RValue a [RValue a b]
parallelPass [] = Done []
parallelPass (Done x    : rs) = fmap (Done x :) (parallelPass rs)
parallelPass (NotDone f : rs) = NotDone $ \a -> fmap (f a :) (parallelPass rs)


kWise :: RValue a [a] -> [RValue a b] -> RValue a [b]
kWise k = fmap findStart $ untilAllDone (genAndUseVector k) . markStart

genAndUseVector k rs = fmap (cyc . useVector rs) k

markStart :: [RValue a b] -> [RValue a (Maybe b)]
markStart = ((Done Nothing) :) . map (fmap Just)

splitAtNothings :: [Maybe a] -> [[a]]
splitAtNothings [] = [[]]
splitAtNothings (Nothing : xs) = [] : splitAtNothings xs
splitAtNothings (Just x : xs) = mapHead (x :) (splitAtNothings xs)

rotate [] = []
rotate (x:xs) = xs ++ [x]

findStart :: [Maybe a] -> [a]
findStart = concat . rotate . splitAtNothings

{-
loopProcess :: ((a,b) -> Maybe (a,b)) -> b -> [a] -> ([a], [a])
loopProcess f 
-}

useVector :: [RValue a b] -> [a] -> ([RValue a b], [RValue a b])

useVector (Done x    : rs) as     = mapFst (Done x :) (useVector rs as)
useVector (NotDone f : rs) (a:as) = mapFst (f a :) (useVector rs as)
useVector rs [] = ([], rs)
useVector [] _  = ([], [])

cyc (xs, ys) = ys ++ xs
-}