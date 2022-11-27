


fibon :: (Eq t, Num t, Num a) => t -> [a]
fibon 1 = [1]
fibon 2 = [1,1]
fibon 3 = fibon 2 ++ [last (fibon 2) + last (fibon 1)]
fibon n = fibon (n-1) ++ [last (fibon (n-1)) + last (fibon (n-2))]

smallfibon :: [a]
smallfibon = fibon n 

