palindrome :: Show a => a -> Bool
palindrome x = show x == reverse (show x)

process :: Integer -> Integer
process x = x + read (reverse (show x))

lychrel :: Integer -> [Integer]
lychrel x =  tail $ take 51 $ iterate process x

answer :: [Integer]
answer = [x | x <- [1..10000], not (any palindrome (lychrel x))]

main :: IO ()
main = do
    print $ length answer