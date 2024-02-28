possiblePath :: Integer
possiblePath = product [21..40] `div` product [2..20]

main :: IO ()
main = do
    print possiblePath