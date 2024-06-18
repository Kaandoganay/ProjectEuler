import Data.List

permuted :: (Show a, Num a) => a -> a -> Bool
permuted x i = sort (show x) == sort (show (i * x))

constraint :: (Show a, Num a, Enum a) => a -> Bool
constraint x = all (permuted x) [2..6]

answer :: Integer
answer = head $ filter constraint [1..]

main :: IO ()
main = do
    print answer