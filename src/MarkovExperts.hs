module MarkovExperts where

import Util(mapPair)
import Experts

data WUTree = Leaf Int
            | Branch Int WUTree WUTree

instance Show WUTree where
  show (Leaf n) = show n
  show (Branch n l r) = show (l,r)

newTree = Branch 2 (Leaf 1) (Leaf 1)

sizeOf :: WUTree -> Int
sizeOf (Leaf n)       = n
sizeOf (Branch n _ _) = n

incTree :: WUTree -> [Bool] -> WUTree
incTree (Leaf n)       _            = Leaf (n + 1)
incTree (Branch n l r) []           = Branch (n + 1) l r
incTree (Branch n l r) (True : bs)  = Branch (n + 1) l (incTree r bs)
incTree (Branch n l r) (False : bs) = Branch (n + 1) (incTree l bs) r

getLocation :: WUTree -> [Bool] -> WUTree
getLocation t              []     = t
getLocation (Leaf n)       (_:_)  = error "Location not found."
getLocation (Branch _ l r) (b:bs) = getLocation (if b then r else l) bs

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
  updateState newB (ME n tree bs) = ME (n - 1) (Branch (2 * sizeOf tree) tree tree) (bs ++ [newB])
  
  updateModel newB (ME 0 tree bs)     = ME 0 (incTree tree (bs ++ [newB])) bs
  updateModel newB (ME n tree bs)     = ME n tree bs


newMarkovExpert :: Int -> MarkovExpert
newMarkovExpert n = ME n newTree []

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
incLocation (Leaf _) []             = Just newTree
incLocation (Leaf _) (_:_)          = error "Location not found"
incLocation (Branch _ _ _) []     = Nothing

{-incLocation (Branch n l r) (b:bs) = 
  case (incLocation (if b then r else l) bs, b) of
    (Nothing, _)       -> Nothing
    (Just newR, True)  -> Just (Branch (n+1) l newR)
    (Just newL, False) -> Just (Branch (n+1) newL r)
-}
incLocation (Branch n l r) (True:bs) =
  fmap (\t -> Branch (n+1) l t) (incLocation r bs)
incLocation (Branch n l r) (False:bs) =
  fmap (\t -> Branch (n+1) t r) (incLocation l bs)

newUniversalExpert :: UniversalExpert
newUniversalExpert = UE newTree []

---------------------------------------

incPair :: (Int, Int) -> Bool -> (Int, Int)
incPair (f, t) True  = (f, t + 1)
incPair (f, t) False = (f + 1, t)

incList :: [(Int, Int)] -> Int -> Bool -> [(Int,Int)]
incList []     _ _  = error "Empty list"
incList (x:xs) 0 b  = (incPair x b) : xs
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
  predict (RE stats (Just (b, n))) = toProb ((indexPair stats b) !! n)
  
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

