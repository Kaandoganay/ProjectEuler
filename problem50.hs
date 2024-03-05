import Data.Numbers.Primes

bak :: [[Integer]]
bak = [take x (drop y ( take 500 primes))| x <-[1..500], y<-[1..500]]

cab :: [[Integer]]
cab = [take x (drop y (reverse (take 500 primes)))| x <-[1..500], y<-[1..500]]

total :: [[Integer]]
total =  bak ++ cab

consequtiveTotal :: [Integer]
consequtiveTotal = filter isPrime (map sum total)

lengthTotal :: Int
lengthTotal = maximum [length xs | xs <-total, sum xs `elem` consequtiveTotal, sum xs < 1000000 ]

thePrime :: [Integer]
thePrime = filter (<1000000)[sum xs| xs <- total, length xs == lengthTotal]