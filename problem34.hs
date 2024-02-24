factorial :: Integer -> Integer
factorial n = product [1..n]


numToDigits :: Integer -> [Integer]
numToDigits 0 = []
numToDigits x = (x `mod` 10) : numToDigits (x `div` 10)

facOfDigits :: Integer -> Integer
facOfDigits n = sum (map factorial (numToDigits n)) 

curious :: Integer
curious = sum [x | x <- [1..50000],x >2 , x == facOfDigits x]

main :: IO ()
main = 
    do
        print curious