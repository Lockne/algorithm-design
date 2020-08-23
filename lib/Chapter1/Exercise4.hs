reverseLnTime :: [a] -> [a]
reverseLnTime = foldl (flip (:)) []
