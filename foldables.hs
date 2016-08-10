-- |
-- Module:        foldables.hs
-- Author:        Steven Ward <stevenward94@gmail.com>
-- URL:           https://github.com/StevenWard94/LearningHaskell.d
-- Last Change:   2016 Aug 09
--

module Foldables where

import Prelude hiding ( foldr, foldl )
import qualified Data.Foldable as F

-- Tree type
data Tree a = Empty | Node a (Tree a) (Tree a)
    deriving (Show, Read, Eq)

instance Functor Tree where
    fmap f Empty = Empty
    fmap f (Node x left right) = Node (f x) (fmap f left) (fmap f right)

singleton :: a -> Tree a
singleton x = Node x Empty Empty

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x Empty = singleton x
treeInsert x (Node a left right)
    | x == a = Node x left right
    | x < a = Node a (treeInsert x left) right
    | x > a = Node a left (treeInsert x right)

treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem x Empty = False
treeElem x (Node a left right)
    | x == a = True
    | x < a = treeElem x left
    | x > a = treeElem x right

-- Now, applying properties of "Foldable" to this class

instance F.Foldable Tree where
--  foldMap :: (Monoid m, Foldable t) => (a -> m) -> t a -> m
    foldMap f Empty = mempty
    foldMap f (Node x l r) = F.foldMap f l `mappend`
                             f x           `mappend`
                             F.foldMap f r
