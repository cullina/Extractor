module RandomValue where


import Control.Monad(replicateM)

data RValue a b = Done b | NotDone (a -> RValue a b)

instance Functor (RValue a) where
    fmap f (Done x)    = Done (f x)
    fmap f (NotDone g) = NotDone $ \a -> fmap f (g a)

instance Monad (RValue a) where 
    return = Done
    
    (Done x)    >>= g = g x
    (NotDone f) >>= g = NotDone $ \a -> f a >>= g  
    

composeRValues :: RValue a b -> RValue b c -> RValue a c

composeRValues _ (Done x) = Done x
  
composeRValues ry (NotDone f) = 
  (composeRValues ry . f) =<< ry
  


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