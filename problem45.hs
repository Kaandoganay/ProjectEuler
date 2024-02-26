pentagon :: [Integer]
pentagon = [n * (3*n-1) `div`2 | n <- [1..]]

hexagon :: [Integer]
hexagon = [n*(2*n-1) | n <- [1..]]

f :: Ord a => [a] -> [a] -> [a]
f (a:as) (b:bs) | a < b     = f as (b:bs)
                | a > b     = f (a:as) bs 
                | otherwise = a : f as bs

solution :: Integer
solution = head $ dropWhile (<=40755) (f pentagon hexagon)

main :: IO ()
main = do 
    print solution