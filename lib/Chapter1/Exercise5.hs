map' :: (a -> b) -> [a] -> [b]
map' f  = foldr (\y ys -> (f y) : ys  ) []

filter' :: (a -> Bool) -> [a] -> [a]
filter' p = foldr (\y ys -> if p y then y : ys else ys) []
