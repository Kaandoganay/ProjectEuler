  
palindrome :: Integer
palindrome = maximum [x*y | x <- [100..999], y <- [100..999], let s = show (x*y), s == reverse s ]
