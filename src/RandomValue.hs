module RandomValue where


data RValue b a = Done a | NotDone(b -> RValue b a)

instance Functor (RValue b) where
    fmap f (Done x)    = Done (f x)
    fmap f (NotDone g) = NotDone $ \b -> fmap f (g b)

instance Monad (RValue b) where 
    return = Done
    
    (Done x)    >>= g = g x
    (NotDone f) >>= g = NotDone $ \b -> f b >>= g  
    
composeRValues :: RValue b c -> RValue a b -> RValue a c
    
composeRValues rx ry = cRV ry rx ry
    where cRV _  (Done x)    _           = Done x
          cRV ry (NotDone f) (Done y)    = cRV ry (f y) ry
          cRV ry (NotDone f) (NotDone g) = NotDone $ \z -> cRV ry (NotDone f) (g z) 
