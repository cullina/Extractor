module DoublyRandomValue where

data DRValue a b c = Done c | NeedA (a -> DRValue a b c) | NeedB (b -> DRValue a b c)

instance Functor (DRValue a b) where
    fmap f (Done x)  = Done (f x)
    fmap f (NeedA g) = NeedA $ \a -> fmap f (g a)
    fmap f (NeedB g) = NeedB $ \b -> fmap f (g b)

instance Monad (DRValue a b) where 
    return = Done
    
    (Done x)  >>= g = g x
    (NeedA f) >>= g = NeedA $ \a -> f a >>= g  
    (NeedB f) >>= g = NeedB $ \b -> f b >>= g  


arrA f = NeedA $ \a -> Done (f a)

arrB f = NeedB $ \b -> Done (f b)


flipSources :: DRValue a b c -> DRValue b a c

flipSources (Done x)  = Done x
flipSources (NeedA g) = NeedB $ \b -> flipSources (g b)
flipSources (NeedB g) = NeedA $ \a -> flipSources (g a)


fromLists (Done x)  as     bs = (x, as, bs)
fromLists (NeedA f) (a:as) bs = fromLists (f a) as bs
fromLists (NeedA f) []     _  = error "Reached end of list of A."
fromLists (NeedB f) as (b:bs) = fromLists (f b) as bs
fromLists (NeedB f) _      [] = error "Reached end of list of B."

attachCounters = aC 0 0
  where aC counterA counterB (Done x) = Done (x, counterA, counterB)
        aC counterA counterB (NeedA f) = NeedA $ \a -> aC (counterA + 1) counterB (f a)
        aC counterA counterB (NeedB f) = NeedB $ \b -> aC counterA (counterB + 1) (f b)