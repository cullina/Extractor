module DoublyRandomValue where

import RandomValue

data DRValue a b c = DDone c | NeedA (a -> DRValue a b c) | NeedB (b -> DRValue a b c)

instance Functor (DRValue a b) where
    fmap f (DDone x) = DDone (f x)
    fmap f (NeedA g) = NeedA $ \a -> fmap f (g a)
    fmap f (NeedB g) = NeedB $ \b -> fmap f (g b)

instance Monad (DRValue a b) where 
    return = DDone
    
    (DDone x) >>= g = g x
    (NeedA f) >>= g = NeedA $ \a -> f a >>= g  
    (NeedB f) >>= g = NeedB $ \b -> f b >>= g  



arrA f = NeedA $ \a -> DDone (f a)

arrB f = NeedB $ \b -> DDone (f b)


fromRV :: RValue a b -> DRValue a c b

fromRV (Done x)    = DDone x
fromRV (NotDone g) = NeedA $ \a -> fromRV (g a)


flipSources :: DRValue a b c -> DRValue b a c

flipSources (DDone x) = DDone x
flipSources (NeedA g) = NeedB $ \b -> flipSources (g b)
flipSources (NeedB g) = NeedA $ \a -> flipSources (g a)


fromLists (DDone x) as     bs = (x, as, bs)
fromLists (NeedA f) (a:as) bs = fromLists (f a) as bs
fromLists (NeedA f) []     _  = error "Reached end of list of A."
fromLists (NeedB f) as (b:bs) = fromLists (f b) as bs
fromLists (NeedB f) _      [] = error "Reached end of list of B."

attachCounters = aC 0 0
  where aC counterA counterB (DDone x) = DDone (x, counterA, counterB)
        aC counterA counterB (NeedA f) = NeedA $ \a -> aC (counterA + 1) counterB (f a)
        aC counterA counterB (NeedB f) = NeedB $ \b -> aC counterA (counterB + 1) (f b)