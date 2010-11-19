choose n 0 = 1
choose 0 k = 0
choose n k = (choose (n-1) (k-1)) * n `div` k


savings n l k = let list = take k $ chooseList (n-1) (l-1)
                in (foldl1 gcd list, last list)