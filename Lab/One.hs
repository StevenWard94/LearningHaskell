{- | Exercises for Lab 1 found at: http://www.inf.ed.ac.uk/teaching/courses/inf1/fp/tutorials/Labweek.pdf
 - Module:        Lab.One
 - Author:        Steven Ward <stevenward94@gmail.com>
 - URL:           https://github.com/StevenWard94/LearningHaskell
 - Last Change:   2016 June 21
 -}

module Lab.One
( square
, isTriple
, leg1
, leg2
, hyp
) where

import Test.QuickCheck


square :: (Real a) => a -> a
square x = x * x

isTriple :: (Real a) => a -> a -> a -> Bool
isTriple a b c = square a + square b == square c

leg1 :: (Real a) => a -> a -> a
leg1 x y = if abs x < abs y
              then leg1 y x
              else square x - square y

leg2 :: (Real a) => a -> a -> a
leg2 x y = abs $ 2 * x * y

hyp :: (Real a) => a -> a -> a
hyp x y = square x + square y

prop_triple :: (Real a) => a -> a -> Bool
prop_triple r t
    | x < y     = prop_triple y x
    | otherwise = isTriple (leg1 x y) (leg2 x y) (hyp x y)
    where (x, y) = (abs r, abs t)
