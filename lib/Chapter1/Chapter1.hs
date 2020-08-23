type Nat = Int

-- | Function that labels the entries of a list
label :: [a] -> [(Nat, a)]
label xs = zip [0..] xs

-- | Function that returns the length of a list
length' :: [a] -> Nat
length' = foldr succ 0 where succ x n = n + 1

-- | Using the appropriate anonymous lambda function
--   we can rewrite the above function as:
length'' :: [a] -> Nat
length'' = foldr (\_ n -> n + 1) 0

-- | Folding a list can be thought of as accumalating values
--   into something finite. The foldr function folds the list
--   from the right to the left, and foldl folds from left to right.
foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' f e [] = e
foldr' f e (x:xs) = f x (foldr' f e xs)

foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' f e [] = e
foldl' f e (x:xs) = foldl' f (f e x) xs

-- | Can we use loops in Haskell?
loopUntil :: (a -> Bool) -> (a -> a) -> a -> a
loopUntil p f x = if p x then x else until p f (f x)

-- | And a while loop would be?
while :: (a -> Bool) -> (a -> a) -> a -> a
while p = loopUntil (not . p)

-- | Let's try to write a function that computes all permutations
--   of a list.


-- | Often two-step processes can be fused into a one-step process.
--   This is called Fusion.
