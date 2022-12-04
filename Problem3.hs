primes :: [Integer]
primes = 2 : 3 : [5,7..]

solution3 :: Integer -> [Integer]
solution3 n = [x | x <- primes, mod n x == 0]