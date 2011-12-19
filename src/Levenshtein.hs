module Levenshtein where


matchPattern (x:xs) (y:ys) = 
    (x == y, map (== x) ys, map (== y) xs) : matchPattern xs ys

matchPattern _ _ = []


updateCosts (c1:c2:cs) (match:ms) cost =
    let upperCost = if match
                    then c1
                    else c2 + 1
        newCost = min cost upperCost
    in if newCost > 0
       then newCost : updateCosts (c2:cs) ms newCost
       else repeat 0

updateCosts _ [] _ = []

updateCosts _ _ _ = error "Cost list too short."





slowLDist (x:xs) (y:ys) = 
    if x == y
    then slowLDist xs ys
    else 1 + min (slowLDist xs (y:ys)) (slowLDist (x:xs) ys)


slowLDist xs [] = length xs

slowLDist [] ys = length ys


levDist x y = fst (quadLDist x y)

quadLDist xs ys = quadLDist' xs ys (0,1) (iterate incFst (1,1))

quadLDist' _ [] _ costs = last costs

quadLDist' xs (y:ys) c costs =
    let matches  = map (==y) xs
        newCosts = buildRow (incFst c) c costs matches
    in quadLDist' xs ys (incFst c) newCosts



buildRow leftCost diagonalCost (c:cs) (m:ms) =
    let c1 = incFst (minCost leftCost c)
        c2 = if m 
             then minCost c1 diagonalCost
             else c1
    in c2 : buildRow c2 c cs ms
    


buildRow _ _ _ _ = []
    
incFst (c, n) = (c+1, n)

minCost (c1, n1) (c2, n2) = 
    case compare c1 c2 of
      EQ -> (c1, n1 + n2)
      LT -> (c1, n1)
      GT -> (c2, n2)
{-
-- xs is a superstring of ys
buildIntermediate (x:_) [] = [x]
buildIntermediate (x:xs) (y:ys) =
  if x == y
  then x : buildIntermediate xs ys
  else x : y : ys
       
       
superstring (x:xs) (y:ys) (z:zs)
  | z /= x     = x : superstring xs (y:ys) (z:zs)
  | z /= y     = y : superstring (x:xs) ys (z:zs)
  | otherwise  = z : superstring xs ys zs
superstring xs ys [] = xs ++ ys

substring (x:xs) (y:ys) (z:zs)
  | z /= x     = substring (x:xs) ys zs
  | z /= y     = substring xs (y:ys) zs
  | otherwise  = z : substring xs ys zs
substring _ _ _ = []
-}