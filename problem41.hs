import Data.Numbers.Primes
import Data.List

pandigital :: [Int]
pandigital = map read (concatMap permutations ["12", "123","1234","12345","123456","1234567","12345678","123456789"])

primePandigital :: [Int]
primePandigital = filter isPrime pandigital

main :: IO ()
main = do
    print $ maximum primePandigital