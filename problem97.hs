nonMersenne :: String
nonMersenne = show (28433 * (2 ^ 7830457) + 1)

lastTenDigits :: String
lastTenDigits = drop 2357197 nonMersenne

main :: IO ()
main = do
    putStrLn lastTenDigits