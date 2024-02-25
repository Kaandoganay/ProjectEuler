import Data.Numbers.Primes

removeDigits :: Integer -> [Integer]
removeDigits n |  n < 10 = []
                |otherwise = (n `div` 10) : removeDigits (n `div` 10)

removeDigits' :: Integer -> [Integer]
removeDigits' n |  n < 10 = []
                |otherwise = (n `mod` 10^(length (show n) - 1) ) : removeDigits' (n `mod` 10^(length (show n) - 1))

addNumber :: Integer -> [Integer]
addNumber n = n : removeDigits n

addNumber' :: Integer -> [Integer]
addNumber' n = n : removeDigits' n

truncatable :: Integer -> Bool
truncatable n | not (all isPrime (addNumber n)) = False
              | otherwise = True

truncatable' :: Integer -> Bool
truncatable' n | not (all isPrime (addNumber' n)) = False
              | otherwise = True

truncatableSet :: [Integer]
truncatableSet = take 11 $ drop 4 [x | x <- primes, truncatable x , truncatable' x]

main :: IO ()
main = print $ sum truncatableSet
