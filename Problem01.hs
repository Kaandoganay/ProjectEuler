

solution1 :: Integer
solution1 = sum [a | a <- [1..999], mod a 3 ==0 || mod a 5 == 0]

