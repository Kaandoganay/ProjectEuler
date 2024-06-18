factorial :: (Eq a, Num a, Enum a) => a -> a
factorial 0 = 1
factorial n = product [1..n]

combinatorics :: Integral a => a -> a -> a
combinatorics n r = factorial n `div` (factorial r * factorial (n-r))

answer :: Int
answer = length [(n,r) | n <- [1..100], r <- [1..100], combinatorics n r > 1000000]

main :: IO ()
main = do
    print answer