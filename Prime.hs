module Prime 
    ( primes 
    , factorize
    ) where


primes = 2 : oddPrimes

oddPrimes = 3 : primeGen 5

primeGen q = 
    if (q ==) . fst . head $ factorize' q oddPrimes
    then q : primeGen (q + 2)
    else primeGen (q + 2)


factorize n = factorize' n primes

factorize' 1 _ = []

factorize' n [] = [(n, 1)]

factorize' n (p:ps) = 
    let (q, k, done) = divideOut n p 0
        rest = if done then [] else ps
    in if k > 0
       then (p, k) : factorize' q rest
       else factorize' q rest
            

divideOut n p k =
    let (q, r) = quotRem n p
        in if r == 0
           then divideOut q p (k + 1)
           else (n, k, p > q)
