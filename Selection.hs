module Selection where


import Bitstream
import Data.List(transpose)





--Set selection

nthElem n = head . drop n


splitEvenly n xs = transpose $ splitEvenly' n xs

splitEvenly' n [] = []
splitEvenly' n xs = let (some, rest) = splitAt n xs
                    in some : splitEvenly' n rest

randomElem xs bs = let l = length xs
                       ((n, m), bs') = uniform l bs
                   in (nthElem n xs, bs')

appendRandomElem (ys, bs) xs = let (y, bs') = randomElem xs bs
                               in (y : ys, bs')




getSubset xs bs k = foldl appendRandomElem ([], bs) $ splitEvenly k xs 
                               

appendRandomSubset xs (subsets, bs) k = let (subset, bs') = getSubset xs bs k
                                        in (subset : subsets, bs')

getSubsets xs bs ks = foldl (appendRandomSubset xs) ([], bs) ks



