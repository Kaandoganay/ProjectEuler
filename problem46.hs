import Data.List
import Data.Numbers.Primes

oddNumbers :: [Integer]
oddNumbers = [3,5..]

compositeNumbers :: [Integer]
compositeNumbers = filter (not . isPrime) oddNumbers


goldbachNumbers :: Integral a => a -> Bool
goldbachNumbers n = any isPrime (takeWhile (>1) $ map (\i -> n - 2*i*i) [1..])


first :: Integer
first = head $ filter (not . goldbachNumbers) compositeNumbers

main :: IO ()
main = do 
    print first
