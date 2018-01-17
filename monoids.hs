-- |
-- Module:        monoids.hs
-- Author:        Steven Ward <stevenward94@gmail.com>
-- URL:           https://github.com/StevenWard94/LearningHaskell.d
-- Last Change:   2016 Aug 03
--

module Monoids where

import Data.Monoid hiding ( Product(..), Sum(..), Any(..), All(..) )

-- re-implementation of Monoid class defined in Data.Monoid
-- this is really just for demonstration purposes...
class AMonoid m where
    amempty :: m            -- mempty is a polymorphic constant representing a monoid's identity value
    amappend :: m -> m -> m   -- mappend is a binary function that takes 2 monoid values and returns a 3rd
    amconcat :: [m] -> m     -- mconcat takes a list of monoid values and applies mappend b/w its elements to return a single value
    amconcat = foldr amappend amempty    -- default implementation for mconcat (works for most instances of Monoid)


------------------------------------------------------------------------------------------
--    MONOID LAWS
--    -----------
-- 1. mempty `mappend` x = x
-- 2. x `mappend` mempty = x
-- 3. (x `mappend` y) `mappend` z = x `mappend` (y `mappend` z)
------------------------------------------------------------------------------------------


-- Lists are monoids...
newtype List a = List { getList :: [a] }
infixl 5 .++
(.++) :: List a -> List a -> List a
(.++) xs ys = List $ (++) (getList xs) (getList ys)

instance Monoid (List a) where
    mempty = List []
    mappend = (.++)


-- Product & Sum
newtype Product a = Product { getProduct :: a }
    deriving ( Eq, Ord, Read, Show, Bounded )

instance (Num a) => Monoid (Product a) where
    mempty = Product 1
    Product x `mappend` Product y = Product (x * y)


newtype Sum a = Sum { getSum :: a }
    deriving ( Eq, Ord, Read, Show, Bounded )

instance (Num a) => Monoid (Sum a) where
    mempty = Sum 0
    Sum x `mappend` Sum y = Sum (x + y)


-- Any & All
newtype Any = Any { getAny :: Bool }
    deriving ( Eq, Ord, Read, Show, Bounded )

instance Monoid Any where
    mempty = Any False
    Any x `mappend` Any y = Any (x || y)


newtype All = All { getAll :: Bool }
    deriving ( Eq, Ord, Read, Show, Bounded )

instance Monoid All where
    mempty = All True
    All x `mappend` All y = All (x && y)


-- Ordering
lengthCompare' :: String -> String -> Ordering
lengthCompare' x y = let a = length x `compare` length y
                         b = x `compare` y
                      in if a == EQ then b else a

-- this works because Ordering is an instance of Monoid where
--     mempty = EQ
--     LT `mappend` _ = LT
--     EQ `mappend` y = y
--     GT `mappend` _ = GT
-- so, if (length x `compare` length y) = EQ, x `compare` y will be used
-- otherwise, (length x `compare` length y) will be used
lengthCompare :: String -> String -> Ordering
lengthCompare x y = (length x `compare` length y) `mappend` (x `compare` y)

vowelLenCompare :: String -> String -> Ordering
vowelLenCompare x y = (length x `compare` length y) `mappend`
                      (vowels x `compare` vowels y) `mappend`
                      (x `compare` y)
                where vowels = length . filter (`elem` "aeiou")


-- Maybe the monoid
data MMaybe a = NNothing | JJust a

instance Monoid a => Monoid (MMaybe a) where
    mempty = NNothing
    NNothing `mappend` mm = mm
    mm `mappend` NNothing = mm
    JJust mm1 `mappend` JJust mm2 = JJust (mm1 `mappend` mm2)


newtype FFirst a = FFirst { getFirst :: Maybe a }
    deriving (Eq, Ord, Read, Show)

instance Monoid (FFirst a) where
    mempty = FFirst Nothing
    FFirst (Just x) `mappend` _ = FFirst (Just x)
    FFirst Nothing `mappend` x = x
