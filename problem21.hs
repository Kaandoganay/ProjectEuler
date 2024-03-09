import Data.Numbers.Primes
import Data.List

d :: Integral a => a -> a
d n = sum [x | x <-[1..(n-1)], n `mod `x == 0]

amicable :: Integral a => a -> a -> Bool
amicable x y = (d x ==  d y) && x < y && d x > 1 && d y > 1

amicablePairs :: [(Integer, Integer)]
amicablePairs = [(x,y) |x <- [1..10000], y <- [1..10000], amicable x y ]