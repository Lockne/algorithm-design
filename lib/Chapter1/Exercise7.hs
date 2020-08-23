takeWhileFold p = foldr (\x xs -> if p x then x : xs else []) []

-- | How does this demonstrate that foldr does not have
--   to process the entire list?
-- | Like so:
-- | Consider takeWhileFold odd [1,2,3]
--   We can expand it as so:
--      (odd 1 (odd 2 (somenoise...
--   According to our definition, odd 2 xs will yield []
--      (odd 1 [])
--   Since 1 is odd, we have this expression yielding 1 : []
--      [1]
--   This is the required answer.
