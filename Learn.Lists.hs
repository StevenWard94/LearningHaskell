{- | A Haskell script for messing around with list contructs
 - File:          Learn.Lists
 - Author:        Steven Ward <stevenward94@gmail.com>
 - URL:           https://github.com/StevenWard94/LearningHaskell
 - Last Change:   2016 June 07
 -}

import Data.List (nub)

factorial' :: Integral a => a -> a
factorial' x
    | x <= 1 = 1
    | otherwise = x * factorial' (x - 1)

factList' :: Integral a => [a] -> [a]
factList' xs = case xs of [] -> error "No factorials for empty list"
                          xs -> [ factorial' n | n <- xs ]

prodComb :: (Num a) => [a] -> [a] -> [a]
prodComb xs ys
    | null xs = error "Lists cannot be empty"
    | null ys = error "Lists cannot be empty"
    | otherwise = quicksort nub [ x * y | x <- xs, y <- ys ]

maximum' :: Ord a => [a] -> a
maximum' [] = error "cannot get maximum of empty list"
maximum' [x] = x
maximum' (x:xs)
    | x > maxTail = x
    | otherwise = maxTail
    where maxTail = maximum' xs

maxximum :: Ord a => [a] -> a
maxximum [] = error "maximum of empty list"
maxximum [x] = x
maxximum (x:xs) = max x (maxximum xs)

replicate' :: (Num i, Ord i) => i -> a -> [a]
replicate' n x
    | n <= 0      = []
    | otherwise = x:replicate' (n - 1) x

take' :: (Num i, Ord i) => i -> [a] -> [a]
take' n _
    | n <= 0   = []
take' _ []    = []
take' n (x:xs) = x : take' (n - 1) xs

flip' :: [a] -> [a]
flip' [] = []
flip' (x:xs) = flip' xs ++ [x]

repeat' :: a -> [a]
repeat' x = x:repeat' x

zip' :: [a] -> [b] -> [(a,b)]
zip' _ [] = []
zip' [] _ = []
zip' (x:xs) (y:ys) = (x,y):zip' xs ys

elem' :: Eq a => a -> [a] -> Bool
elem' a [] = False
elem' a (x:xs)
    | a == x    = True
    | otherwise = a `elem'` xs

quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
    let smallerSorted = quicksort [a | a <- xs, a <= x]
        biggerSorted = quicksort [a | a <- xs, a > x]
    in smallerSorted ++ [x] ++ biggerSorted
