joiner :: [Integer] -> String
joiner xs = if null xs then "" else show (head xs) ++ joiner (tail xs)


d :: Int -> [Integer] -> Int
d n  xs = read [joiner xs !!n] :: Int

champernowne :: [Integer] -> Int
champernowne xs = d 1 xs * d 10 xs * d 100 xs * d 1000 xs * d 10000 xs * d 100000 xs * d 1000000 xs

main :: IO ()
main = do
    print (champernowne [0,1..] )