import Data.List(transpose, foldl')
import System.Random

bitsToInt :: [Bool] -> Int

bitsToInt = foldl doubleIf 0

doubleIf a b = if b
               then 2*a + 1
               else 2*a


intToBits 0 = []
intToBits n = let (q,r) = quotRem n 2
              in (r == 1) : intToBits q


intWToBits 0 n = []

intWToBits w n = let (q,r) = quotRem n 2
                 in (r == 1) : intWToBits (w - 1) q


maxInBits n = reverse (intToBits (n - 1))


dumbUniform max bs = dumbUniform' max (maxInBits max) bs

dumbUniform' max mm bs = let (x, bs') = popPush mm [] bs
                             try = bitsToInt (reverse x)
                         in if try < max
                            then (try, bs')
                            else dumbUniform' max mm bs'

uniform max bs = let mm = maxInBits max
                     (out, bs') = uniform' mm mm [] bs
                 in (bitsToInt (reverse out), bs')

uniform' _ [] xs bs = (xs, bs)

uniform' mm (m:ms) xs bs = let (b, bs') = getBit bs 
                            in if m
                               then if b
                                    then uniform' mm ms (b:xs) bs'    {- append and continue -}
                                    else popPush ms (b:xs) bs'        {- append and continue without further checks -}
                               else if b
                                    then uniform' mm mm [] bs'        {- discard and restart -}
                                    else uniform' mm ms (b:xs) bs'    {- append and continue -}

popPush [] xs bs = (xs, bs)

popPush (m:ms) xs bs = let (b, bs') = getBit bs
                       in  popPush ms (b:xs) bs'


vNUnbias bs = let (b1, bs') = getBit bs
                  (b2, bs'') = getBit bs'
              in if b1 == b2
                 then vNUnbias bs''
                 else (b2, bs'')


nthElem n = head . drop n


splitEvenly n xs = transpose $ splitEvenly' n xs

splitEvenly' n [] = []
splitEvenly' n xs = let (some, rest) = splitAt n xs
                    in some : splitEvenly' n rest

randomElem xs bs = let l = length xs
                       pair = uniform l bs
                   in (nthElem (fst pair) xs, snd pair)

appendRandomElem (ys, bs) xs = let (y, bs') = randomElem xs bs
                               in (y : ys, bs')




getSubset xs bs k = foldl appendRandomElem ([], bs) $ splitEvenly k xs 
                               

appendRandomSubset xs (subsets, bs) k = let (subset, bs') = getSubset xs bs k
                                        in (subset : subsets, bs')

getSubsets xs bs ks = foldl (appendRandomSubset xs) ([], bs) ks



getBit ((b:bs), n) = (b, (bs, n+1))

bitStream w = attachCounter . (concatMap (intWToBits w)) . intStream . mkStdGen

attachCounter x = (x, 0)


intStream :: RandomGen r => r -> [Int]

intStream gen = let (int, newGen) = random gen
                in int : intStream newGen

generateList f bs = let (x, bs') = f bs
                    in x : generateList f bs'

generateFiniteList f n bs = generateFiniteList' f n ([], bs)

generateFiniteList' f 0 p = p

generateFiniteList' f n (xs, bs) = let (x, bs') = f bs
                                   in generateFiniteList' f (n - 1) (x:xs, bs')

histogramUpdate [] 0 = [1]

histogramUpdate [] n = 0 : histogramUpdate [] (n - 1)

histogramUpdate (h:hs) 0 = (h + 1) : hs

histogramUpdate (h:hs) n = h : histogramUpdate hs (n - 1)

histogram :: [Int] -> [Int]

histogram = foldl' histogramUpdate []