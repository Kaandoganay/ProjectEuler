import Data.Numbers.Primes

consequtivePrime :: [[Integer]]
consequtivePrime = [take y (drop x primes)| x <- [1..321], y <- [1..321]]

sumPrimes :: [Integer]
sumPrimes = filter (<= 1000000) (map sum consequtivePrime)

primeSum :: [Integer]
primeSum = filter isPrime sumPrimes

lengthPrimes :: Int
lengthPrimes = maximum [length xs | xs <- consequtivePrime, sum xs `elem` primeSum]

thePrime :: [Integer]
thePrime = [sum xs |
              xs <- consequtivePrime, length xs == lengthPrimes, sum xs `elem` primeSum]
