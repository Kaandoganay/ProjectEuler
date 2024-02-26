pentagonal :: [Integer]
pentagonal = [n * (3*n -1) `div` 2 | n <- [1..5000]]


pairpentagonal :: (Integer, Integer)
pairpentagonal = head [(a,b) |a<-pentagonal, b<-takeWhile (<a) pentagonal, a+b `elem` pentagonal, a-b `elem` pentagonal]

main :: IO ()
main = do
    print (uncurry (-) pairpentagonal)