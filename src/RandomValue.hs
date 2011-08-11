module RandomValue where


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
    

preMap _ (Done x)    = Done x
preMap f (NotDone g) = NotDone $ (\a -> preMap f (g a)) . f

composeRValues :: RValue a b -> RValue b c -> RValue a c

composeRValues _ (Done x) = Done x
  
composeRValues ry (NotDone f) = 
  (composeRValues ry . f) =<< ry
  


fmapD :: (Functor f) => f (a -> b) -> a -> f b

fmapD r x = fmap ($ x) r 

arr :: (a -> b) -> RValue a b 

arr f = NotDone $ \a -> Done (f a)


untilSuccess :: RValue a (Maybe b) -> RValue a b

untilSuccess f = maybe (untilSuccess f) Done =<< f 


useFixedNumber n = replicateM n $ arr id

pP = mapM (const (arr id))


fromList (Done x)    as     = (x, as)
fromList (NotDone f) (a:as) = fromList (f a) as
fromList (NotDone _) []     = error "Reached end of list."

attachCounter = aC 0
    where aC counter (Done x)    = Done (x, counter)
          aC counter (NotDone f) = NotDone $ \a -> aC (counter + 1) (f a)
          

useInput (Done x) = const (Done x)
useInput (NotDone f) = f

toMaybe (Done x)    = Just x
toMaybe (NotDone _) = Nothing

parallel :: [RValue b a] -> RValue [b] [a]

parallel rs =
  case sequence (map toMaybe rs) of
    Just xs -> Done xs
    Nothing -> NotDone $ \bs -> parallel (zipWith useInput rs bs)