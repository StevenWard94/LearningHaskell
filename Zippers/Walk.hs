-- | Module:        Zippers.Walk                                      \begin1
--   Author:        Steven Ward <stevenward94@gmail.com>
--   URL:           https://github.com/StevenWard94/LearningHaskell.d
--   Last Change:   2016 Aug 07
--

module Zippers.Walk where

import Data.Char ( isDigit, isSpace )
import Data.List.Split ( splitWhen )
import Prelude hiding ( sum )


-- I. Making and Using a Tree Data Structure \begin2
--

data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show)

-- Example Tree... \begin3
egTree :: Tree Char
egTree =
    Node 'P'
        (Node 'O'
            (Node 'L'
                (Node 'N' Empty Empty)
                (Node 'T' Empty Empty)
            )
            (Node 'Y'
                (Node 'S' Empty Empty)
                (Node 'A' Empty Empty)
            )
        )
        (Node 'L'
            (Node 'W'
                (Node 'C' Empty Empty)
                (Node 'R' Empty Empty)
            )
            (Node 'A'
                (Node 'A' Empty Empty)
                (Node 'C' Empty Empty)
            )
        )

-- Here is 'egTree' represented visually:
{-
                     P

           O                    L
      L         Y         W          A
    N    T    S    A    C    R    A    C
-}
-- \end3

-- How could we change, for example, the 'W' in egTree to a 'P'?

-- One way would be to pattern match on the tree (traversing Node,Right,Left)
-- until the desired element is found and then to change that element...
changeToP :: Tree Char -> Tree Char
changeToP (Node x l (Node y (Node _ m n) r)) = Node x l (Node y (Node 'P' m n) r)

-- A less confusing method of accomplishing this could have the function accept
-- a Tree and a list of directions (either L or R) and then change the element
-- found by following those directions...
data Direction = L | R deriving (Show)
type Directions = [Direction]

changeToP' :: Directions -> Tree Char -> Tree Char
changeToP' (L:ds) (Node x l r) = Node x (changeToP' ds l) r
changeToP' (R:ds) (Node x l r) = Node x l (changeToP' ds r)
changeToP' [] (Node _ l r) = Node 'P' l r

-- To avoid printing out the entire tree, we define a function that takes a list
-- of directions and returns the element at the destination reached via those
-- directions...
elemAt :: Directions -> Tree a -> a
elemAt (L:ds) (Node _ l _) = elemAt ds l
elemAt (R:ds) (Node _ _ r) = elemAt ds r
elemAt [] (Node x _ _) = x

-- Quick test of changeToP'
newTree :: Directions -> Tree Char -> Char
newTree ds tree = elemAt ds $ changeToP' ds tree

-- \end

-- II. A Trail of Breadcrumbs \begin2
--

-- INTRODUCTION \begin3
--
-- Although the algorithm in changeToP' works well for small trees, it is
-- actually very inefficient for large trees, especially when making multiple
-- changes. With changeToP', if we change an element at the bottom of a large
-- tree and then want to change another element right next to it, we have to
-- start from the top of the tree all over again!

-- What if we start at the root of the tree and move either left or right,
-- leaving a trail of 'breadcrumbs' with each step? So, when we go left, we
-- remember that we went left and when we go right, we remember that we went
-- right.
--                \end3

type Crumbs = [Direction]

moveLeft :: (Tree a, Crumbs) -> (Tree a, Crumbs)
moveLeft (Node _ l _, crumbs) = (l, L:crumbs)

moveRight :: (Tree a, Crumbs) -> (Tree a, Crumbs)
moveRight (Node _ _ r, crumbs) = (r, R:crumbs)

-- This operator function makes "walking" the tree clearer
(|>) :: t1 -> (t1 -> t) -> t
x |> f = f x
infixl 9 |>

-- \end

-- III. Going Back Up \begin2
--

-- INTRODUCTION \begin3
--
-- In general, each breadcrumb should contain all the data needed to reconstruct
-- its parent node. So, it should have the information from all the paths we
-- didn't take AND it should know the direction we did take, BUT it must not
-- contain the sub-tree that we are currently focusing on, as this sub-tree is
-- the first component of the (Tree a, Breadcrumbs) tuple and we do not want
-- duplicate data.
--                 \end3

-- To have the breadcrumbs also contain information about everything we have
-- "ignored" while traversing the tree, we need to modify our Crumbs type
-- synonym. To do this, we will replace the Direction data type with a new one...
data Crumb a = LeftCrumb a (Tree a) | RightCrumb a (Tree a) deriving (Show)
type Breadcrumbs a = [Crumb a]

goLeft :: (Tree a, Breadcrumbs a) -> (Tree a, Breadcrumbs a)
goLeft (Node x l r, crumbs) = (l, LeftCrumb x r:crumbs)

goRight :: (Tree a, Breadcrumbs a) -> (Tree a, Breadcrumbs a)
goRight (Node x l r, crumbs) = (r, RightCrumb x l:crumbs)

-- By remembering information about the parent nodes and untraveled paths, we
-- have gained the ability to actually go back up the tree...
goUp :: (Tree a, Breadcrumbs a) -> (Tree a, Breadcrumbs a)
goUp (t, LeftCrumb x r:crumbs) = (Node x t r, crumbs)
goUp (t, RightCrumb x l:crumbs) = (Node x l t, crumbs)

-- \end

-- IV. Manipulating Trees Under Focus \begin2
--

-- INTRODUCTION \begin3
--
-- With the pair (Tree a, Breadcrumbs a), we have all the necessary information
-- to reconstruct the entire tree AND to have a focus on a sub-tree. Such a pair
-- that contains a focused part of a data structure and its surroundings is
-- called a "zipper", because moving focus up and down the data structure
-- resembles the operation of a zipper...
type Zipper a = (Tree a, Breadcrumbs a)                      -- \end3

-- Now that we are able to move up and down a Tree type, let's define a function
-- that modifies the element in the root of a "zipper-focused sub-tree"...
modify :: (a -> a) -> Zipper a -> Zipper a
modify f (Node x l r, crumbs) = (Node (f x) l r, crumbs)
modify f (Empty,crumbs) = (Empty,crumbs)

-- This function takes the focused, empty sub-tree of a Zipper and replaces it
-- with a non-empty sub-tree, thus attaching a Tree to a leaf node...
attach :: Tree a -> Zipper a -> Zipper a
attach t (_, crumbs) = (t, crumbs)

-- Here is a function that just runs to the top of a Tree, regardless of the
-- currently focused sub-tree...
topMost :: Zipper a -> Zipper a
topMost (t,[]) = (t,[])
topMost z = topMost $ goUp z

-- \end

-- V. Focusing On Lists \begin2
--

infixr 5 :>
data List a = List | a :> (List a)

singleton :: a -> List a
singleton = \x -> x:>List

instance (Show a) => Show (List a) where
    show List = "{ }"
    show (x:>List) = "{ " ++ show x ++ " }"
    show (x:>xs) = "{ " ++ show x ++ " " ++ (showTail $ toList xs)
                where
                  showTail ls = case ls of
                      []      -> " }"
                      [l]     -> show l
                      (l:ls)  -> show l ++ " " ++ showTail ls

instance (Read a) => Read (List a) where
    readsPrec _ input
      | null result = [(List, "")]
      | otherwise = [(fromList result, "")]
      where
        result = map read (filter (not . null) (splitWhen isSpace (filter (\c -> isDigit c || isSpace c) input)))

instance (Eq a) => Eq (List a) where
    list1 == list2 = toList list1 == toList list2

instance Monoid (List a) where
    {-# INLINE mempty #-}
    mempty = List
    {-# INLINE mappend #-}
    mappend = (+>)
    {-# INLINE mconcat  #-}
    mconcat xss = fromList [ x | xs <- xss, x <- (toList xs) ]

instance (Ord a) => Ord (List a) where
    List `compare` List = EQ
    List `compare` _    = LT
    _    `compare` List = GT

(+>) :: List a -> List a -> List a
(+>) List    ys = ys
(+>) (x:>xs) ys = x :> xs +> ys

toList :: List a -> [a]
toList List = []
toList (x:>List) = [x]
toList (x:>xs) = x : toList xs

fromList :: [a] -> List a
fromList [] = List
fromList [x] = x:>List
fromList (x:xs) = x :> fromList xs
