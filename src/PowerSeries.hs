module PowerSeries where

import Data.List(unfoldr)
import Util((...))

zipWithDefaults :: (a -> b -> c) -> (a -> c) -> (b -> c) -> [a] -> [b] -> [c]
zipWithDefaults f g h (x:xs) (y:ys) = f x y : zipWithDefaults f g h xs ys
zipWithDefaults _ g _ xs     []     = map g xs
zipWithDefaults _ _ h []     ys     = map h ys

reverseInits :: [a] -> [[a]]
reverseInits xs = unfoldr f ([], xs)
  where
    f :: ([a], [a]) -> Maybe ([a], ([a],[a]))
    f (_,  [])   = Nothing
    f (ys, x:xs) = 
      let ys' = x:ys
      in Just (ys', (ys',xs))

-- only works if second argument is infinite
simpleConvolve :: [a] -> [b] -> [[(a,b)]]
simpleConvolve xs = map (zip xs) . reverseInits

--takes an infinite list of infinite lists and gives an infinite list of finite lists
diagonalize :: [[a]] -> [[a]]
diagonalize = foldr f []
  where
    f :: [a] -> [[a]] -> [[a]]
    f []     ys = [] : ys
    f (x:xs) ys = [x] : zipWithDefaults (:) (: []) id xs ys

grid :: [a] -> [b] -> [[(b,a)]]
grid xs = map (\y -> map ((,) y) xs)

-- works on finite and infinite arguments in both places.
convolve :: [a] -> [b] -> [[(b,a)]]
convolve = diagonalize ... grid
          
seriesMult :: (Num a) => [a] -> [a] -> [a]
seriesMult = map (sum . map (uncurry (*))) ... convolve

seriesAdd  :: (Num a) => [a] -> [a] -> [a]
seriesAdd = zipWithDefaults (+) id id

seriesOne :: [Int]
seriesOne = 1 : repeat 0

seriesZero :: [Int]
seriesZero = repeat 0

seriesShift = (:) 0

data PS = PS [Int]

instance Num PS where
  (+) (PS xs) (PS ys) = PS (seriesAdd xs ys)
  (*) (PS xs) (PS ys) = PS (seriesMult xs ys)
  negate (PS xs)      = PS (map negate xs)
  signum              = undefined
  abs                 = undefined
  fromInteger n       = PS [fromInteger n]
  

data EOPS = Zero
          | Both [Int]
          | Even [Int]
          | Odd [Int]
          deriving Show
            

instance Num EOPS where
  (+)           = eoAdd
  (*)           = eoMult
  negate        = eoMap negate
  signum        = undefined
  abs           = undefined
  fromInteger 0 = Zero
  fromInteger n = Even [fromInteger n]
            
-- f(x^2) + x g(x^2)
alternate :: [a] -> [a] -> [a]
alternate = concat ... zipWith f
  where
    f x y = [x,y]

evenPart :: [a] -> [a]
evenPart []  = []
evenPart [x] = [x] 
evenPart (x:_:z) = x : evenPart z

oddPart :: [a] -> [a]
oddPart []  = []
oddPart [_] = [] 
oddPart (_:y:z) = y : oddPart z

split :: EOPS -> (EOPS, EOPS)
split Zero      = (Zero, Zero)
split (Even xs) = (Even xs, Zero)
split (Odd  xs) = (Zero, Odd xs)
split (Both xs) = (Even (evenPart xs), Odd (oddPart xs))

expand :: EOPS -> [Int]
expand Zero      = repeat 0
expand (Both xs) = xs
expand (Even xs) = alternate xs (repeat 0)
expand (Odd  xs) = alternate (repeat 0) xs

eoAdd :: EOPS -> EOPS -> EOPS
eoAdd Zero      y         = y
eoAdd x         Zero      = x
eoAdd (Even xs) (Even ys) = Even (seriesAdd xs ys)
eoAdd (Odd  xs) (Odd  ys) = Odd  (seriesAdd xs ys)
eoAdd x         y         = Both (seriesAdd (expand x) (expand y))

eoMult :: EOPS -> EOPS -> EOPS
eoMult Zero      _         = Zero
eoMult _         Zero      = Zero
eoMult (Even xs) (Even ys) = Even (seriesMult xs ys)
eoMult (Even xs) (Odd  ys) = Odd  (seriesMult xs ys)
eoMult (Odd  xs) (Even ys) = Odd  (seriesMult xs ys)
eoMult (Odd  xs) (Odd  ys) = Even (0 : seriesMult xs ys)
eoMult x         y         = Both (seriesMult (expand x) (expand y))

eoMap :: (Int -> Int) -> EOPS -> EOPS
eoMap _ Zero      = Zero
eoMap f (Even xs) = Even (map f xs)
eoMap f (Odd  xs) = Odd  (map f xs)
eoMap f (Both xs) = Both (map f xs)

eoShift Zero      = Zero
eoShift (Even xs) = Odd xs
eoShift (Odd  xs) = Even $ 0 : xs
eoShift (Both xs) = Both $ 0 : xs