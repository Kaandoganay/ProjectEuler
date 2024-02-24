baseTwo :: Integer -> [Integer]
baseTwo 0 = []
baseTwo x = (x `mod` 2) : baseTwo (x `div` 2)

baseTen :: Integer -> [Integer] 
baseTen 0 = []
baseTen x = (x `mod` 10) : baseTen (x `div` 10)

palondromicBaseTwo :: [Integer]
palondromicBaseTwo = [x |x <- [1..1000000], baseTwo x == reverse (baseTwo x)]

palondromicBaseTen :: [Integer]
palondromicBaseTen =[x |x <- [1..1000000], baseTen x == reverse (baseTen x)]

palondromicBoth :: [Integer]
palondromicBoth = [x | x<-[1..1000000], x `elem` palondromicBaseTwo, x `elem` palondromicBaseTen]

sumOfAll :: Integer
sumOfAll = sum palondromicBoth

main :: IO ()
main = do
    print sumOfAll