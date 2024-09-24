import Data.List

triangleNumbers :: [Int]
triangleNumbers = [x * (x+1) `div` 2 | x <- [1..]]

divisors :: Integral a => a -> [a]
divisors n = (1:) $ nub $ concat [ [x, div n x] | x <- [2..limit], rem n x == 0 ]
     where limit = (floor.sqrt.fromIntegral) n

numOfdivisors :: Int-> Int
numOfdivisors x = length $ divisors x

highlyDivisable :: [Int] -> Int
highlyDivisable (x:xs) = if numOfdivisors x > 500 then 500 else highlyDivisable xs

problem12 :: IO ()
problem12 = do
    print $ highlyDivisable triangleNumbers