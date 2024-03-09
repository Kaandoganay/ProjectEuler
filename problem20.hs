import Data.Char

factorial :: (Show a, Num a, Enum a) => a -> String
factorial n = show (product [1..n])

factorialDigitSum :: (Show a, Num a, Enum a) => a -> Int
factorialDigitSum n  = sum $ map digitToInt (factorial n)

main :: IO ()
main = do
    print $ factorialDigitSum 100