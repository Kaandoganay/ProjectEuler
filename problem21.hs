d :: Integral a => a -> a
d n = sum [x | x <-[1..(n-1)], n `mod `x == 0] 