import Data.List

pandigital :: [String]
pandigital = permutations "123456789"

multiple :: [[Integer]]
multiple = [map (*n) [1..9]| n <- [1..999]]