import Data.Char
import System.IO

import Data.List


order :: Char -> Int
order x = ord x - 64


nameScores :: [[Char]] -> [[Int]]
nameScores = map (map order)


sumScores :: [[Char]] -> [Int]
sumScores xs = map sum (nameScores xs)

total :: [[Char]] -> Int
total xs= sum $ zipWith (*) (sumScores  xs) [1..(length xs)]


main :: IO ()
main = do
  name <- readFile "0022_names.txt"
  let names = sort $ read ("["++ name++"]")
  print $ total names