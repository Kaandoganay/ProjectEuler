power :: [Integer]
power = [x^y | x <- [1..9], y <- [1..22] , length (show (x^y)) == y]

answer :: Int
answer = length power

main :: IO ()
main = do
    print answer