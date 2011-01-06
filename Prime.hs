module Prime 
    ( primes 
    , testDivisors
    , factorize
    ) where


primes = 2 : oddPrimes

oddPrimes = 3 : primeGen 5

primeGen q = 
    if null $ testDivisors q oddPrimes
    then q : primeGen (q + 2)
    else primeGen (q + 2)


testDivisors n [] = []
 
testDivisors n (p:ps) = 
    let (q, r) = quotRem n p
    in if r == 0
       then p : testDivisors n ps
       else if q > p
            then testDivisors n ps
            else []


factorize n = factorize' n primes

factorize' 1 _ = []

factorize' n pp@(p:ps) = 
    let (q, r) = quotRem n p
    in if r == 0
       then p : factorize' q pp
       else if q > p
            then factorize' n ps
            else [n]
            
