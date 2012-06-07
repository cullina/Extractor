module Circular where

data Circular a = Circular [a] [a]

toList (Circular xs ys) = xs ++ ys

fromList = Circular []

instance Functor Circular where
  fmap f (Circular xs ys) = Circular (map f xs) (map f ys)