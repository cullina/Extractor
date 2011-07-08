module RandomStream where

import RandomValue

data RStream a b = Out (RStream a b) b | In (a -> RStream a b)

instance Functor (RStream a) where
  fmap f (Out rs b) = Out (fmap f rs) (f b)
  fmap f (In g)     = In $ \a -> fmap f (g a)





pipeList (Out rs b) as     = b : pipeList rs as
pipeList (In f)     (a:as) = pipeList (f a) as
pipeList (In _)     []     = error "Reached end of list."


takeRS 0 _          = Done []
takeRS n (Out rs b) = fmap (b :) (takeRS (n - 1) rs) 


markov :: (b -> RValue a b) -> RValue a b -> RStream a b

markov trans (Done x) = Out (markov trans (trans x)) x
markov trans (NotDone f) = In $ \a -> markov trans (f a)

repeatRV :: RValue a b -> RStream a b

repeatRV rV = rRV rV rV
  where rRV rV (Done x) = Out rV x
        rRV rV (NotDone f) = In $ \a -> rRV rV (f a) 