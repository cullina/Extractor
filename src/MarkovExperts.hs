module MarkovExperts where

import Util(mapPair)
import SubsetSelection(getSubset)
import Experts
import Bit(bitsToInt)

data WUTree = Leaf Int
            | Branch Int WUTree WUTree

instance Show WUTree where
  show (Leaf n) = show n
  show (Branch n (Leaf l) (Leaf r)) = show (toProb (l,r), l+r)
  show (Branch n l r) = show (l,r)

newTree :: Int -> WUTree
newTree 0 = Leaf 1
newTree n = 
  let t = newTree (n-1) 
  in Branch (2 * sizeOf t) t t

sizeOf :: WUTree -> Int
sizeOf (Leaf n)       = n
sizeOf (Branch n _ _) = n

incTree :: WUTree -> [Bool] -> WUTree
incTree t = f t . reverse
  where 
    f (Leaf n)       _            = Leaf (n + 1)
    f (Branch n l r) []           = Branch (n + 1) l r
    f (Branch n l r) (True : bs)  = Branch (n + 1) l (f r bs)
    f (Branch n l r) (False : bs) = Branch (n + 1) (f l bs) r

getLocation :: WUTree -> [Bool] -> WUTree
--getLocation t              []     = t
--getLocation (Leaf n)       (_:_)  = error "Location not found."
--getLocation (Branch _ l r) (b:bs) = getLocation (if b then r else l) bs
getLocation = foldr f 
  where
    f b (Branch _ l r) = if b then r else l
    f _ (Leaf _)       = error "Location not found."

lookupOdds :: WUTree -> [Bool] -> (Int, Int)
lookupOdds t bs = case getLocation t bs of 
  (Leaf _)       -> error "Location is leaf."
  (Branch _ l r) -> (sizeOf l, sizeOf r)

toProb :: (Int, Int) -> Double                              
toProb (a,b) = fromIntegral b / fromIntegral (a + b)

data MarkovExpert = ME Int WUTree [Bool] 
                  deriving Show

instance Expert MarkovExpert where
  predict (ME n tree state) = toProb $ lookupOdds tree state
  
  updateState newB (ME 0 tree [])     = ME 0 tree []
  updateState newB (ME 0 tree (b:bs)) = ME 0 tree (bs ++ [newB])
  updateState newB (ME n tree bs)     = ME (n - 1) (Branch (2 * sizeOf tree) tree tree) (bs ++ [newB])
  
  updateModel newB (ME 0 tree bs)     = ME 0 (incTree tree (newB : bs)) bs
  updateModel newB (ME n tree bs)     = ME n tree bs


newMarkovExpert :: Int -> MarkovExpert
newMarkovExpert n = ME n (newTree 1) []

--------------------------------

data RestrictedMarkovExpert = RME Int [Bool] WUTree [Bool]
                              deriving Show

instance Expert RestrictedMarkovExpert where
  predict (RME n mask tree state) = toProb . lookupOdds tree $ getSubset state mask
  
  updateState newB (RME n mask tree bs) = RME n mask tree (take n (newB : bs))
  
  updateModel newB (RME n mask tree bs) = RME n mask (incTree tree (newB : getSubset bs mask)) bs
  
newRestrictedMarkovExpert :: [Bool] -> RestrictedMarkovExpert
newRestrictedMarkovExpert mask = 
  let n = length mask
      m = 1 + length (filter id mask)
  in RME n mask (newTree m) []
                              

--------------------------------

data UniversalExpert = UE WUTree [Bool]

instance Expert UniversalExpert where
  predict (UE tree state) = toProb $ lookupOdds tree state
  
  updateState = undefined
  updateModel = undefined
  
  update newB (UE tree bs) = 
    case incLocation tree newBs of 
      Nothing   -> UE tree newBs
      Just newT -> UE newT []  
    where newBs = bs ++ [newB] 


incLocation :: WUTree -> [Bool] -> Maybe WUTree
incLocation (Leaf _) []       = Just (newTree 1)
incLocation (Leaf _) (_:_)    = error "Location not found"
incLocation (Branch _ _ _) [] = Nothing

{-incLocation (Branch n l r) (b:bs) = 
  case (incLocation (if b then r else l) bs, b) of
    (Nothing, _)       -> Nothing
    (Just newR, True)  -> Just (Branch (n+1) l newR)
    (Just newL, False) -> Just (Branch (n+1) newL r)
-}
incLocation (Branch n l r) (True:bs) =
  fmap (Branch (n+1) l) (incLocation r bs)
incLocation (Branch n l r) (False:bs) =
  fmap (flip (Branch (n+1)) r) (incLocation l bs)

newUniversalExpert :: UniversalExpert
newUniversalExpert = UE (newTree 1) []

---------------------------------------

incPair :: (Int, Int) -> Bool -> (Int, Int)
incPair (f, t) True  = (f, t + 1)
incPair (f, t) False = (f + 1, t)

incList :: [(Int, Int)] -> Int -> Bool -> [(Int,Int)]
incList []     _ _  = error "Empty list"
incList (x:xs) 0 b  = incPair x b : xs
incList (x:xs) n b  = x : incList xs (n - 1) b 

count :: [Bool] -> Int
count = length . filter id

data MarkovSumExpert = MSE Int [(Int, Int)] [Bool]

instance Expert MarkovSumExpert where
  predict (MSE n stats state) = toProb (stats !! count state)
  
  updateState newB (MSE 0 stats [])     = MSE 0 stats []
  updateState newB (MSE 0 stats (b:bs)) = MSE 0 stats (bs ++ [newB])
  updateState newB (MSE n stats bs)     = MSE (n - 1) ((1,1) : stats) (bs ++ [newB])
                                            
  updateModel newB (MSE 0 stats bs)     = MSE 0 (incList stats (count bs) newB) bs
  updateModel newB (MSE n stats bs)     = MSE n stats bs
                                            
newMarkovSumExpert :: Int -> MarkovSumExpert
newMarkovSumExpert n = MSE n [(1,1)] []

---------------------------------------

incQuad :: Bool -> ((Int, Int), (Int, Int)) -> Bool -> ((Int, Int), (Int, Int))
incQuad b (x, y) False = (incPair x b, y)
incQuad b (x, y) True  = (x, incPair y b)

pairProduct :: Double -> [(Int, Int)] -> (Double, Double)
pairProduct p = mapPair ((** p) . product . map fromIntegral) . unzip

toProbD :: (Double, Double) -> Double                              
toProbD (a,b) = b / (a + b)



indexPair :: (a, a) -> Bool -> a
indexPair (x, _) False = x
indexPair (_, y) True = y


data MarkovLinearExpert = MLE Double Int [((Int, Int),(Int,Int))] [Bool]

instance Show MarkovLinearExpert where
  show (MLE _ _ stats state) = show stats

instance Expert MarkovLinearExpert where
  predict (MLE p n stats state) = toProbD . pairProduct p $ zipWith indexPair stats state
  
  updateState newB (MLE p 0 stats [])     = MLE p 0 [] []
  updateState newB (MLE p 0 stats (b:bs)) = MLE p 0 (zipWith (incQuad newB) stats (b:bs)) (bs ++ [newB])
  updateState newB (MLE p n stats bs)     = MLE p (n - 1) (((1,1),(1,1)) : stats) (bs ++ [newB])
                                            
  updateModel newB (MLE p n stats bs)     = MLE p n (zipWith (incQuad newB) stats bs) bs
                                            
newMarkovLinearExpert :: Int -> MarkovLinearExpert
newMarkovLinearExpert n = MLE (recip (fromIntegral n)) n [] []

--------------------------------------------

data RunsExpert = RE ([(Int,Int)],[(Int,Int)]) (Maybe (Bool, Int))

instance Show RunsExpert where
  show (RE stats state) = show (mapPair (take 10) stats)

instance Expert RunsExpert where
  predict (RE stats Nothing)     = 0.5
  predict (RE stats (Just (b, n))) = toProb (indexPair stats b !! n)
  
  updateState newB (RE stats Nothing)       = RE stats (Just (newB, 0))
  updateState newB (RE stats (Just (b, n))) = RE stats (Just (newB, if b == newB then n + 1 else 0))

  updateModel newB (RE stats Nothing)       = RE stats Nothing
  updateModel newB (RE stats (Just (b, n))) = RE (incListPair stats b n newB) (Just (b,n)) 

incListPair :: ([(Int,Int)],[(Int,Int)]) -> Bool -> Int -> Bool -> ([(Int,Int)],[(Int,Int)])
incListPair (f,t) False n newB = (incList f n newB, t)
incListPair (f,t) True  n newB = (f, incList t n newB)
  
newRunsExpert :: RunsExpert
newRunsExpert = RE (repeat (1,1), repeat (1,1)) Nothing

-------------------------------------------------


data FixedMarkovExpert = FME Int [Bool] [Double] [Bool] 
                       deriving Show

instance Expert FixedMarkovExpert where
  predict (FME n mask vector state) = vector !! bitsToInt (getSubset state mask)
  
  updateState newB (FME n mask vector bs) = FME n mask vector (take n (newB : bs))
  
  updateModel newB  = id


newFixedMarkovExpert :: Int -> FixedMarkovExpert
newFixedMarkovExpert 0 = FME 3 [True, True, True] [0.9, 0.633, 0.633, 0.367, 0.633, 0.367, 0.367, 0.1] []
newFixedMarkovExpert 1 = FME 5 [True, False, True, False, True] [0.4, 0.3, 0.2, 0.4, 0.7, 0.2, 0.5, 0.3] []
newFixedMarkovExpert 2 = FME 4 [False, True, True, True] [0.65, 0.45, 0.45, 0.25, 0.45, 0.25, 0.25, 0.05] []
newFixedMarkovExpert 3 = FME 3 [True, True, True]        [0.75, 0.55, 0.3, 0.1, 0.9, 0.5, 0.3, 0.6] []
newFixedMarkovExpert 4 = FME 4 [False, True, True, True] [0.4, 0.6, 0.8, 0.2, 0.6, 0.8, 0.4, 0.2] []
newFixedMarkovExpert 5 = FME 3 [True, True, True]        [0.2, 0.4, 0.3, 0.5, 0.2, 0.2, 0.7, 0.7] []
newFixedMarkovExpert 6 = FME 3 [True, True, True]        (replicate 8 0.2) []
newFixedMarkovExpert 7 = FME 3 [True, True, True]        (replicate 8 0.5) []
newFixedMarkovExpert 8 = FME 3 [True, True, True]        (replicate 8 0.9) []
