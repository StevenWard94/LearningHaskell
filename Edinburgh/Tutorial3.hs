-- vim: set foldlevel=1:
-- |
-- Module:        Edinburgh.Tutorial3
-- Author:        Steven Ward <stevenward94@gmail.com>
-- URL:           https://github.com/StevenWard94/LearningHaskell.d
-- Last Change:   2016 Aug 14
--

-- This module consists of my (attempted) solutions to the third Edinburgh Tutorial:
--                      HIGHER-ORDER FUNCTIONS
--        Informatics 1 - Functional Programming: Tutorial 3
--
-- the instructions for which can be found at:
--      http://www.inf.ed.ac.uk/teaching/courses/inf1/fp/#haskell
--
--------------------------------------------------------------------------------
--
module Edinburgh.Tutorial3 where

import Test.QuickCheck
import Data.Char ( toUpper, isAlpha )

-- Map \begin

-- INTRODUCTION \begin2
-- Transforming every list element by a particular function is a common need
-- when processing lists — for example, we may want to...
--   –  add one to each element of a list of numbers
--   –  extract the first element of every pair in a list
--   –  convert every character in a string to uppercase
--   –  add a grey background to every picture in a list of pictures
-- The 'map' function captures this pattern, allowing us to avoid repititious
-- code that results from writing a recursive function for each case.
-- Consifer a function, g, defined in terms of an imaginary function f, as follows:
--     g []     = []
--     g (x:xs) = f x : g xs
-- This definition could also be written, equivalently, using 'map':
--     g xs = map f xs
-- Here is the definition of 'map':
--     map :: (a -> b) -> [a] -> [b]
--     map f []      = []
--     map f (x:xs)  = f x : map f xs
-- Given 'map' and a function that operates on a single element, we can easily
-- write a function that operatres on a list. For instance, the function that
-- extracts the first element of every pair can be defined as follows (using
-- fst :: (a,b) -> a):
--     fsts :: [(a,b)] -> [a]
--     fsts pairs = map fst pairs
--                                          \end2

-- Exercise 1    \begin2
-- Using map and other suitable library functions, write definitions for the
-- following...

-- 1. (a) A function, uppers, that converts a string to uppercase.
uppers :: String -> String
uppers str = map toUpper str

-- 1. (b) A function, doubles, that doubles every item in a list.
doubles :: [Int] -> [Int]
doubles xs = map (*2) xs

-- 1. (c) A function, penceToPounds, that turns prices given in pence into the
-- same price given in pounds
penceToPounds :: [Int] -> [Float]
penceToPounds = map $ (/100) . fromIntegral

-- 1. (d) Write a list-comprehension version of 'uppers' and use it to check the
-- answer to (a).
uppers' :: String -> String
uppers' str = [ toUpper ch | ch <- str ]

-- \end2
-- \end

-- Filter \begin

-- INTRODUCTION \begin2
-- Removing elements from a list is another common need. For example, we might
-- want to remove non-alphabetic characters from a string, or negative integers
-- from a list. This pattern is captured by the 'filter' function.
-- Here is the definition of 'filter':
--     filter :: (a -> Bool) -> [a] -> [a]
--     filter p []     = []
--     filter p (x:xs) | p x       = x : filter p xs
--                     | otherwise = filter p xs
-- \end2

-- Exercise 2 \begin2
-- Using 'filter' and other standard library functions, write definitions for
-- the following...

-- 2. (a) A function, alphas, that removes all non-alphabetic characters from
-- a string.
alphas :: String -> String
alphas str = filter isAlpha str

-- 2. (b) A function, rmChar, that removes all occurrences of a character from
-- a string.
rmChar :: Char -> String -> String
rmChar c str = filter (== c) str

-- 2. (c) A function, above, that removes all numbers less than or equal to
-- a given number.
above :: (Num a, Ord a) => a -> [a] -> [a]
above n xs = filter (> n) xs

-- 2. (d) A function, unequals, that removes all pairs (x,y) where x == y
unequals :: (Num a, Eq a) => [(a,a)] -> [(a,a)]
unequals = filter $ uncurry (/=)

-- 2. (e) Write a list-comprehension version of 'rmChar' and use QuickCheck to
-- test it against the version using 'filter'
rmChar' :: Char -> String -> String
rmChar' x str = [ c | c <- str, c /= x ]

-- \end2
-- \end

-- Comprehensions, map and filter \begin

-- As we have seen, list comprehensions process a list using transformations
-- similar to 'map' and 'filter'. In general, [f x | x <- xs, p x] is equivalent
-- to, map f $ filter p xs

-- Exercise 3 \begin2
-- Write expressions equivalent to the following using 'map' and filter. Use
-- QuickCheck to verify your answers.

-- 3. (a) [toUpper c | c <- s, isAlpha c]
upperChars' :: String -> String
upperChars' s = [ toUpper c | c <- s, isAlpha c ]

upperChars :: String -> String
upperChars s = map toUpper $ filter isAlpha s

prop_upperChars :: String -> Bool
prop_upperChars s = upperChars' s == upperChars s


-- 3. (b) [2 * x | x <- xs, x > 3]
lgDoubles' :: [Int] -> [Int]
lgDoubles' xs = [ 2 * x | x <- xs, x > 3 ]

lgDoubles :: [Int] -> [Int]
lgDoubles xs = map (*2) $ filter (> 3) xs

prop_lgDoubles :: [Int] -> Bool
prop_lgDoubles xs = lgDoubles' xs == lgDoubles xs


-- 3. (c) [reverse s | s <-strs, even (length s)]
reverseEvenLen' :: [String] -> [String]
reverseEvenLen' strs = [ reverse s | s <- strs, even (length s) ]

reverseEvenLen :: [String] -> [String]
reverseEvenLen strs = map reverse $ filter (even . length) strs

prop_reverseEvenLen :: [String] -> Bool
prop_reverseEvenLen strs =
    reverseEvenLen' strs == reverseEvenLen strs

-- \end2
-- \end
