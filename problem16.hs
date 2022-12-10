


digits :: (Integral b, Num a) => b -> a
digits n = 2^n

sumd :: Integral p => p -> p
sumd 0 = 0
sumd x = (x `mod` 10) + sumd (x `div` 10)



sumdigits :: (Integral p, Integral b) => b -> p
sumdigits n = sumd (digits n) 
