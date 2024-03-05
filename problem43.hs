import Data.List
import Data.Char

allPermutations :: [String]
allPermutations = permutations "0123456789"

d :: Int -> [a] -> a
d n xs = xs !!(n-1)

number1 :: [Char] -> Int
number1 xs= read [d 2 xs, d 3 xs, d 4 xs] :: Int

number2 :: [Char] -> Int
number2 xs= read [d 3 xs, d 4 xs, d 5 xs] :: Int

number3 :: [Char] -> Int
number3 xs= read [d 4 xs, d 5 xs, d 6 xs] :: Int

number4 :: [Char] -> Int
number4 xs= read [d 5 xs, d 6 xs, d 7 xs] :: Int

number5 :: [Char] -> Int
number5 xs= read [d 6 xs, d 7 xs, d 8 xs] :: Int

number6 :: [Char] -> Int
number6 xs= read [d 7 xs, d 8 xs, d 9 xs] :: Int

number7 :: [Char] -> Int
number7 xs= read [d 8 xs, d 9 xs, d 10 xs] :: Int

true1 :: [Char] -> Bool
true1 xs = even (number1 xs)

true2 xs= number2 xs `mod` 3 == 0

true3 xs= number3 xs `mod` 5 == 0

true4 xs = number4 xs `mod` 7 == 0

true5 xs = number5 xs `mod` 11 == 0

true6 xs = number6 xs `mod` 13 == 0

true7 xs = number7 xs `mod` 17 == 0

pandigital :: [String]
pandigital = [xs | xs <- allPermutations, true1 xs, true2 xs, true3 xs, true4 xs, true5 xs, true6 xs, true7 xs]

sumPandigital :: [String] -> Int
sumPandigital [] = 0
sumPandigital pandigital= (read (head pandigital) :: Int) + sumPandigital (tail pandigital)