
calculate :: (Num a, Read a) => [Char] -> [a]
calculate xs = [product (map (\c -> read [c]) (take 13 (drop i xs))) | i <- [0..length xs - 13]]


findLargestProduct :: String -> Int
findLargestProduct xs = maximum [product (map (\c -> read [c] :: Int) group) | group <- adjacent 13 xs]
  where
    adjacent n xs = [take n (drop i xs) | i <- [0..length xs - n]]