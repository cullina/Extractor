module CompositeExperts where

import Experts

data ExpWExpert a = EWE Double (Bool -> Double -> Double) Double [(a, Double, Double)]

instance Show (ExpWExpert a) where
  show = show . weightVector 

instance (Expert a) => Expert (ExpWExpert a) where
  predict (EWE _ _ p _)    = p
  
  updateState b (EWE beta l _ es) = EWE beta l (weightedPred newEs) newEs
    where
      newEs  = map (nextPrediction b) es
      nextPrediction b (ex, _, w) = 
        let newEx = updateState b ex
        in (newEx, predict newEx, w)
      
      
  updateModel b (EWE beta l p es) = EWE beta l p newEs
    where
      newEs  = map (rescaleWeight totalWeight) tempEs
      totalWeight = recip . sum . map weight $ tempEs
      tempEs = map (updateWeight l b) es
      updateWeight lossFn b (ex, p, w) = (updateModel b ex, p, w * exp (beta * lossFn b p)) 
      rescaleWeight factor (ex, p, w) = (ex, p, w * factor)

weightedPred :: [(a, Double, Double)] -> Double
weightedPred = sum . map contribution

contribution :: (a, Double, Double) -> Double
contribution (_, p, w) = p * w

weight :: (a, Double, Double) -> Double
weight       (_, _, w) = w
      
weightVector :: ExpWExpert a -> [Double]
weightVector (EWE _ _ _ es) = map weight es

newExpWExpert :: Expert a => Double -> (Bool -> Double -> Double) -> [a] -> ExpWExpert a
newExpWExpert beta l es = EWE beta l (weightedPred newEs) newEs
  where
    initW = recip . fromIntegral $ length es
    newEs = map f es
    f ex  = (ex, predict ex, initW)
