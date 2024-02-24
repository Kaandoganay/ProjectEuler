import Data.List

allpermutations :: [[Char]]
allpermutations = permutations "0123456789"

millionthpermutation :: [Char]
millionthpermutation = sort allpermutations !! 999999

main :: IO ()
main = do
    print (read millionthpermutation :: Int)