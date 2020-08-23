wrap :: a -> [a]
wrap a = [a]

unwrap :: [a] -> Maybe a
unwrap [x] = Just x
unwrap _ = Nothing


single :: [a] -> Bool
single [x] = True
single _ = False
