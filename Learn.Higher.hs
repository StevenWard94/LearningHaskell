{- | A Haskell script to mess around with higher-order functions
 - Module:          Learn.Higher
 - Author:          Steven Ward <stevenward94@gmail.com>
 - URL:             https://github.com/StevenWard94/LearningHaskell
 - Last Change:     2016 June 07
 -}

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

xflip' :: (a -> b -> c) -> (b -> a -> c)
xflip' f = g
    where g x y = f y x

flip' :: (a -> b -> c) -> b -> a -> c
flip' f y x = f x y

