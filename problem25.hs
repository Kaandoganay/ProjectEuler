fibs :: [Integer]
fibs = 1:1:zipWith (+) fibs (tail fibs)

fib :: Int -> Integer
fib n = fibs !!(n-1)

makeInteger :: [String]
makeInteger = map show fibs

firstInteger :: Integer
firstInteger = read $ head [x | x <- makeInteger, length x == 1000]

indexInteger :: String
indexInteger = show . length $ takeWhile (<= firstInteger) fibs

main :: IO ()
main = do
    putStrLn indexInteger