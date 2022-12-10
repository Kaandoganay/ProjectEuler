import Data.Ord ( comparing )
import Data.List ( maximumBy )


chain :: Integer -> [Integer]
chain 1 = [1]
chain n = if even n then n: chain (div n 2) else n:chain ((3*n)+1)

listchain :: Integer -> [[Integer]]
listchain n = map chain [1..n]



maxchain :: Integer -> Integer
maxchain n = head ( maximumBy (comparing length) (listchain n) )