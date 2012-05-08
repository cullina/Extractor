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
  
  update (ME 0 tree [])     newB = ME 0 (incTree tree [newB]) []
  update (ME 0 tree (b:bs)) newB = ME 0 (incTree tree (b : newBs)) newBs 
    where newBs = bs ++ [newB] 
  update (ME n tree bs)     newB = ME (n - 1) (Branch (2 * sizeOf tree) tree tree) (bs ++ [newB])


newMarkovExpert :: Int -> MarkovExpert
newMarkovExpert n = ME n newTree []

--------------------------------

data UniversalExpert = UE WUTree [Bool]

instance Expert UniversalExpert where
  predict (UE tree state) = toProb $ lookupOdds tree state
  
  update (UE tree bs) newB = 
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
  
  update (MSE 0 stats [])     newB = MSE 0 (incList stats 0 newB) []
  update (MSE 0 stats (b:bs)) newB = MSE 0 (incList stats (count (b:bs)) newB) (bs ++ [newB])
  update (MSE n stats bs)     newB = MSE (n - 1) ((1,1) : stats) (bs ++ [newB])
                                            
newMarkovSumExpert :: Int -> MarkovSumExpert
newMarkovSumExpert n = MSE n [(1,1)] []

---------------------------------------

incQuad :: Bool -> ((Int, Int), (Int, Int)) -> Bool -> ((Int, Int), (Int, Int))
incQuad b (x, y) False = (incPair x b, y)
incQuad b (x, y) True  = (x, incPair y b)

pairProduct :: [(Int, Int)] -> (Int, Int)
pairProduct = mapPair product . unzip

indexPair :: (a, a) -> Bool -> a
indexPair (x, _) False = x
indexPair (_, y) True = y


data MarkovLinearExpert = MLE Int [((Int, Int),(Int,Int))] [Bool]

instance Expert MarkovLinearExpert where
  predict (MLE n stats state) = toProb . pairProduct $ zipWith indexPair stats state
  
  update (MLE 0 stats [])     newB = MLE 0 [] []
  update (MLE 0 stats (b:bs)) newB = MLE 0 (zipWith (incQuad newB) stats (b:bs)) (bs ++ [newB])
  update (MLE n stats bs)     newB = 
    MLE (n - 1) (((1,1),(1,1)) : zipWith (incQuad newB) stats bs) (bs ++ [newB])
                                            
newMarkovLinearExpert :: Int -> MarkovLinearExpert
newMarkovLinearExpert n = MLE n [] []

--------------------------------------------

data RunsExpert = RE ([(Int,Int)],[(Int,Int)]) (Maybe (Bool, Int))

instance Expert RunsExpert where
  predict (RE stats Nothing)     = 0.5
  predict (RE stats (Just (b, n))) = toProb ((indexPair stats b) !! n)
  
  update (RE stats Nothing)       newB = RE stats (Just (newB, 0))
  update (RE stats (Just (b, n))) newB = RE newStats newRun 
    where 
      newStats = incListPair stats b n newB
      newRun = Just (newB, if b == newB then n + 1 else 0)

incListPair :: ([(Int,Int)],[(Int,Int)]) -> Bool -> Int -> Bool -> ([(Int,Int)],[(Int,Int)])
incListPair (f,t) False n newB = (incList f n newB, t)
incListPair (f,t) True  n newB = (f, incList t n newB)
  
newRunsExpert :: RunsExpert
newRunsExpert = RE (repeat (1,1), repeat (1,1)) Nothing

           
           