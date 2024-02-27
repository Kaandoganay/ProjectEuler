selfPower :: Integer-> [Integer]
selfPower n = [n^n | n <- [1..n]]

lastN :: Int -> String -> String
lastN n xs = drop (length xs - n ) xs


main :: IO ()
main = do
    putStrLn $  lastN 10 (show (sum (selfPower 1000)))