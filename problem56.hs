powers :: [Integer]
powers =[x^y| x <-[1..100], y <-[1..100]]

sumd :: Integral t => t -> t
sumd 0 = 0
sumd x = (x `mod` 10) + sumd (x `div` 10)

sumPowers :: [Integer]
sumPowers = map sumd powers

maxPower :: Integer
maxPower = maximum sumPowers

main :: IO ()
main = do
    print maxPower