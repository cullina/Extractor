choose n 0 = 1
choose 0 k = 0
choose n k = (choose (n-1) (k-1)) * n `div` k

chooseList n k = let (a,b) = chooseList' n k
                 in a:b

chooseList' n 0 = (1,[])
chooseList' 0 k = (0, [])
chooseList' n k = let (a,b) = chooseList' (n-1) (k-1)
                  in (a * n `div` k, a:b)

savings n l k = let list = take k $ chooseList (n-1) (l-1)
                in (foldl1 gcd list, last list)