import Data.Numbers.Primes

divisors :: Integral a => a -> [a]
divisors x = [y | y <-[2..x], x `mod` y == 0]

relativelyPrime :: Integral a => a -> a -> Bool
relativelyPrime x y = not (any (`elem` divisors x) (divisors y))

relativelyPrimes :: Integral a => a -> [a]
relativelyPrimes x = [y | y <- [1..(x-1)], relativelyPrime x y]