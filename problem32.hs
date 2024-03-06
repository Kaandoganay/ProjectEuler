import Data.List
import Control.Monad

combs :: (Num t, Eq t, Eq a) => t -> [a] -> [([a], [a])]
combs 0 xs = [([],xs)]
combs n xs = [(y:ys,rest) | y <- xs, (ys,rest) <- combs (n-1) (delete y xs)]

l2n :: (Integral a) => [a] -> a
l2n = foldl' (\a b -> 10*a+b) 0

swap :: (b, a) -> (a, b)
swap (a,b) = (b,a)

explode :: (Integral a) => a -> [a]
explode = unfoldr (\a -> if a==0 then Nothing else Just . swap $ quotRem a 10)

pandigiticals :: [Integer]
pandigiticals =
  nub $ do (beg,end) <- combs 5 [1..9]
           n <- [1,2]
           let (a,b) = splitAt n beg
               res = l2n a * l2n b
           guard $ sort (explode res) == end
           return res

main :: IO ()
main= do 
    print $ sum pandigiticals