module Queue where

data Queue a = Queue [a] [a] deriving (Show)


newQueue = Queue [] []

empty (Queue [] []) = True

empty _ = False

enq (Queue xs ys) x = Queue (x:xs) ys


deq (Queue [] []) = Nothing

deq (Queue xs (y:ys)) = Just (y, Queue xs ys)

deq (Queue xs []) = deq (Queue [] (reverse xs))


peek   (Queue [] []) = Nothing

peek q@(Queue xs (y:ys)) = Just (y, Queue xs ys, q)

peek   (Queue xs []) = peek (Queue [] (reverse xs))
