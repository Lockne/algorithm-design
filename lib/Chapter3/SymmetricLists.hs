{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module SymmetricLists
    ( toSL
    , fromSL
    , consSL
    , snocSL
    , headSL
    , lastSL
    , tailSL
    , initSL
    , emptySL
    )
     where

data SymList a = SL [a] [a] deriving (Show)

single :: [a] -> Bool
single [x] = True
single _   = False

toSL :: [a] -> SymList a
toSL xs = SL us vs
          where (us, vs) = fmap reverse $ splitAt (length xs `div` 2) xs

fromSL :: SymList a -> [a]
fromSL (SL xs ys) = xs ++ reverse ys

-- | null  xs => null ys || single ys
-- | null ys => nulll xs || single xs

-- | We want our implementation to satisfy the following:
--   cons   x . fromSL = fromSL . cons x
--   snoc   x . fromSL = fromSL . snoc x
--   last     . fromSL = fromSL . last
--   head     . fromSL = fromSL . head
--   tail     . fromSL = fromSL . tail
--   init     . fromSL = fromSL . init

emptySL :: SymList a
emptySL = SL [] []

consSL :: a -> SymList a -> SymList a
consSL a (SL xs ys) = if null ys then SL [a] xs else SL (a:xs) ys

snocSL :: a -> SymList a -> SymList a
snocSL a (SL xs ys) = if null xs then SL ys [a] else SL xs (a:ys)

lastSL :: SymList a -> a
lastSL (SL xs ys) = if null ys
                    then if null xs
                         then error "Empty Symmetric List"
                         else head xs
                    else head ys

headSL :: SymList a -> a
headSL (SL xs ys) = if null xs
                    then if null ys
                         then error "Empty Symmetric List"
                         else head ys
                    else head xs

tailSL :: SymList a -> SymList a
tailSL (SL xs ys) | null xs = if null ys
                              then error "Empty Symmetric List"
                              else emptySL
                  | single xs = SL (reverse vs) us
                  | otherwise = SL (tail xs) ys
                  where (us, vs) = splitAt (length ys `div` 2) ys

initSL :: SymList a -> SymList a
initSL (SL xs ys) | null ys = if null xs
                              then error "Empty Symmetric List"
                              else emptySL
                  | single ys = SL xs []
                  | otherwise = SL xs (init ys)

inits :: [a] -> [[a]]
inits [] = [[]]
inits (x:xs) = [] : map (x:) (inits xs)

instance Functor (SymList) where
    fmap f (SL a b) = SL (fmap f a) (fmap f b)

instance Semigroup (SymList a) where
    (SL [] []) <> (SL cs ds) = SL cs ds
    (SL cs ds) <> (SL [] []) = SL cs ds
    (SL as bs) <> (SL cs ds) = SL (as <> reverse bs) (ds <> reverse cs)

instance Monoid (SymList a) where
    mempty = SL [] []
    mappend = (<>)
