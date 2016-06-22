{- | Exercises for Lab 1 found at: http://www.inf.ed.ac.uk/teaching/courses/inf1/fp/tutorials/Labweek.pdf
 - Module:        Lab.One
 - Author:        Steven Ward <stevenward94@gmail.com>
 - URL:           https://github.com/StevenWard94/LearningHaskell
 - Last Change:   2016 June 20
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
isTriple 0 _ _ = error "value, 'a', cannot be 0"
isTriple _ 0 _ = error "value, 'b', cannot be 0"
isTriple _ _ 0 = error "value, 'c', cannot be 0"
isTriple a b c = square a + square b == square c

leg1 :: (Real a) => a -> a -> a
leg1 0 _ = error "values cannot be 0 - first value provided was 0"
leg1 _ 0 = error "values cannot be 0 - second value provided was 0"
leg1 x y = if abs x < abs y
              then leg1 y x
              else square x - square y

leg2 :: (Real a) => a -> a -> a
leg2 x y
    | abs x < abs y = leg2 y x
    | result == 0    = error "values cannot be 0"
    | otherwise     = abs result
    where result = 2 * y * x

hyp :: (Real a) => a -> a -> a
hyp 0 _ = error "values cannot be 0 - first value provided was 0"
hyp _ 0 = error "values cannot be 0 - second value provided was 0"
hyp x y = if abs x < abs y
             then hyp y x
             else square x + square y

prop_triple :: (Real a) => a -> a -> Bool
prop_triple x y
    | r < s     = prop_triple s r
    | otherwise = isTriple (leg1 r s) (leg2 r s) (hyp r s)
    where (r, s) = (abs x, abs y)
