diagonal :: Integer -> [Integer]
diagonal 1 = [1]
diagonal 2 = tail $ take 5 $ iterate (+2) (last (diagonal 1))
diagonal n = tail $ take 5 $ iterate (+(2*(n-1))) (last (diagonal (n-1)))

sumOfDiagonals :: Integer -> Integer
sumOfDiagonals n = sum $ concatMap diagonal [1..n]

main :: IO ()
main = do 
    print $ sumOfDiagonals 501