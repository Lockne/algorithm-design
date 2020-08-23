uncons :: [a] -> Maybe (a, [a])
uncons [] = Nothing
uncons (x:xs) = Just $ (x, xs)
