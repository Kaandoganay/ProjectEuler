chain :: Integer -> [Integer]
chain 1 = [1]
chain n = if even n then n: chain (div n 2) else n:chain ((3*n)+1)
