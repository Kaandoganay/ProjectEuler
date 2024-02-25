import Data.Numbers.Primes
import Data.List

circle :: [c] -> [[c]]
circle xs = let trim ys = zipWith const ys xs
            in trim . map trim . iterate tail . cycle $ xs
circle' :: Integer -> [Integer]
circle' x = map read $ circle $ show x

circularPrimes :: [Integer]
circularPrimes = [x | x <- [1 .. 1000000], all isPrime (circle' x)]

main :: IO ()
main = do
    print $ length circularPrimes