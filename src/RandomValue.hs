module RandomValue where


import Control.Monad(replicateM)

data RValue b a = Done a | NotDone (b -> RValue b a)

instance Functor (RValue b) where
    fmap f (Done x)    = Done (f x)
    fmap f (NotDone g) = NotDone $ \b -> fmap f (g b)

instance Monad (RValue b) where 
    return = Done
    
    (Done x)    >>= g = g x
    (NotDone f) >>= g = NotDone $ \b -> f b >>= g  
    

composeRValues :: RValue a b -> RValue b c -> RValue a c

composeRValues _ (Done x) = Done x
  
composeRValues ry (NotDone f) = 
  (composeRValues ry . f) =<< ry
  


arr f = NotDone $ \b -> Done (f b)


untilSuccess :: RValue b (Maybe a) -> RValue b a

untilSuccess f = maybe (untilSuccess f) Done =<< f 


useFixedNumber n = replicateM n $ arr id

pP = mapM (const (arr id))



fromList (Done x) bs = (x, bs)

fromList (NotDone f) (b:bs) = fromList (f b) bs

fromList (NotDone _) [] = error "Reached end of list."

attachCounter = aC 0
    where aC counter (Done x)    = Done (x, counter)
          aC counter (NotDone f) = NotDone $ \b -> aC (counter + 1) (f b)