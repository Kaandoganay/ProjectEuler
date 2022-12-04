
--ilk önce 100 için çözeceğim. Eğer yapabilirsem daha sonra girdileri bizim yazacağımız bir versiyon yapmaya çalışırım.--

firstsum :: Integer
firstsum = sum (map (^2) [1..100])

secondsum :: Integer
secondsum = sum [1..100]^2

sumdifference :: Integer
sumdifference =  secondsum - firstsum 

--İlk işi yaptım şimdi sırada ikincisi.--

firstsum' :: (Num a, Enum a) => a -> a
firstsum' n = sum (map (^2) [1..n])


secondsum' :: (Num a, Enum a) => a -> a
secondsum' n = sum [1..n] ^2

sumdifference' :: (Num a, Enum a) => a -> a
sumdifference' x  = secondsum' x - firstsum' x