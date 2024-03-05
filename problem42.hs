import Data.Char
import System.IO

import Data.List

triangle :: [Integer]
triangle = [(x * (x+1)) `div` 2| x <-[1..2000]]

order :: Char -> Int
order x = ord x - 64

nameScores :: [[Char]] -> [[Int]]
nameScores = map (map order)

sumScores :: [[Char]] -> [Int]
sumScores xs = map sum (nameScores xs)

