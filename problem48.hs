selfPower :: Integer-> [Integer]
selfPower n = [n^n | n <- [1..n]]

lastN :: Int -> String -> String
lastN n xs = drop (length xs - n ) xs


main :: IO ()
main = do
    putStrLn $  lastN 10 (show (sum (selfPower 1000)))

--another solution

sumofnumbers :: [Integer]
sumofnumbers = zipWith (^) [1..1000] [1..1000]

answer :: Integer
answer = sum sumofnumbers `mod` (10^10)