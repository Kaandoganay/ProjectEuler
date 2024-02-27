numbers :: [String]
numbers = ["one","two","three","four","five","six","seven","eight", "nine","ten","eleven","twelve","thirteen","fourteen","fifteen",  "sixteen","seventeen","eighteen", "nineteen"]

tenNumbers :: [String]
tenNumbers = ["twenty","thirty","forty","fifty","sixty","seventy","eighty","ninety"]

countLetter :: Int -> Int
countLetter x | x == 0 = 0
              | x < 20 = length (numbers !! (x-1))
              | x >= 20 && x < 100 = length (tenNumbers !! ((x `div` 10)-2) )  + countLetter (x `mod` 10)
              | x >= 100 && x `mod` 100 == 0 && x<1000 = countLetter (x `div` 100) + length "hundred"
              | x < 1000 = countLetter (x `div` 100) + length "hundred" + length "and" + countLetter (x `mod` 100)
              | x == 1000 = length "onethousand"


total :: Int
total = sum (map countLetter [1..1000])

main :: IO ()
main = do
    print total
