import Data.Numbers.Primes
import Data.List

fourDigitPrimes :: [Integer]
fourDigitPrimes = filter isPrime [1000..9999]

thePrimes :: [[Integer]]
thePrimes = [[x, y, z] |
               x <- fourDigitPrimes,
               y <- filter (> x) fourDigitPrimes,
               sort (show x) == sort (show y),
               z <- filter (> y) fourDigitPrimes,
               sort (show y) == sort (show z),
                z == 2 * y - x]

main :: IO ()
main = do
   print $ concatMap show (last thePrimes)