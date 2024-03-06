import Data.List
import Data.Char(intToDigit)

pandigital :: [String]
pandigital = permutations "123456789"


multiple :: [[Integer]]
multiple =[map (*n) [1..k]| n <- [1..10000], k <- [1,2,3]]

joiner :: [Integer] -> String
joiner = concatMap show


concatMultiple :: [String]
concatMultiple = filter (`elem` pandigital) (map joiner multiple)

makeInt :: Read Int => [String] -> [Int]
makeInt = map read

main :: IO ()
main = do
  print $ maximum ( makeInt concatMultiple)