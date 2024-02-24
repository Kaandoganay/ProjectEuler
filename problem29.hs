import Data.List ( sort ) 

powerSet :: [Integer]
powerSet = [a^b | a <- [2..100], b <- [2..100]]

removedups :: Ord a => [a] -> [a]
removedups xs = remove $ sort xs
  where
    remove []  = []
    remove [x] = [x]
    remove (x1:x2:xs)
      | x1 == x2  = remove (x1:xs)
      | otherwise = x1 : remove (x2:xs)

lengthOfPowers :: Int
lengthOfPowers = length $ removedups powerSet

main :: IO ()
main = do
    print lengthOfPowers