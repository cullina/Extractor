module CompositeExperts where

import Experts

data ExpWExpert a = EWE Double (Bool -> Double -> Double) Double [(a, Double, Double)]

instance Show (ExpWExpert a) where
  show = show . weightVector 

instance (Expert a) => Expert (ExpWExpert a) where
  predict (EWE _ _ p _)    = p
  update  (EWE beta l _ es) b = EWE beta l (weightedPred newEs) newEs
    where
      newEs  = map (nextPrediction b . rescaleWeight totalWeight) tempEs
      totalWeight = recip . sum . map weight $ tempEs
      tempEs = map (updateWeight (l b)) es
      updateWeight lossFn (ex, p, w) = (ex, p, w * exp (beta * lossFn p)) 
      rescaleWeight factor (ex, p, w) = (ex, p, w * factor)
      nextPrediction b (ex, _, w) = 
        let newEx = update ex b
        in (newEx, predict newEx, w)

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
