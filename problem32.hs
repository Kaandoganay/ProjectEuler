import Data.List

pandigital :: [String]
pandigital = permutations "123456789"

pandigitalProducts xs = concatMap show xs `elem` pandigital

set :: [[Integer]]
set = [ [x,y,z]| x <- [1..1000], y <- [1..1000], let z = x*y,pandigitalProducts [x,y,z]]


