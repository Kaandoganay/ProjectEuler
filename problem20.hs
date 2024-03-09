import Basement.IntegralConv

factorial :: (Show a, Num a, Enum a) => a -> String
factorial n = show (product [1..n])


digitSum :: [Char] -> [Int]
digitSum "" = [0]
digitSum xs = (charToInt (head xs) - 48) : digitSum (tail xs)

factorialDigitSum :: (Show a, Num a, Enum a) => a -> Int
factorialDigitSum n = sum $ digitSum (factorial n)

