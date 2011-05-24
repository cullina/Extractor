module BinaryFieldTransform where

import BinaryField
import Data.List(foldl')


innerProduct charPoly u v =
    foldl' polySum (zeroPoly (polyLen charPoly)) $ zipWith (polyProduct charPoly) u v


transform charPoly vect = map (innerProduct charPoly vect) (transformMatrix charPoly)


transformMatrix charPoly =
    let row = multGroup charPoly
        iter = zipWith (polyProduct charPoly) row
        ones = replicate (length row) (head row)
    in cyclicSubgroup ones iter


multGroup charPoly = 
    cyclicSubgroup (onePoly (polyLen charPoly)) (timesX charPoly)


cyclicSubgroup one iter =
    one : cyclicSubgroup' one iter (iter one)

cyclicSubgroup' one iter a
    | a == one  = []
    | otherwise = a : cyclicSubgroup' one iter (iter a)

