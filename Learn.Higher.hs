{- | A Haskell script to mess around with higher-order functions
 - Module:          Learn.Higher
 - Author:          Steven Ward <stevenward94@gmail.com>
 - URL:             https://github.com/StevenWard94/LearningHaskell
 - Last Change:     2016 June 08
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

map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' p (x:xs)
    | p x       = x : filter' p xs
    | otherwise = filter' p xs

-- another way to define Learn.Lists (quicksort)
qsort :: (Ord a) => [a] -> [a]
qsort [] = []
qsort (x:xs) =
    let ltSorted = qsort (filter' (<=x) xs)
        gtSorted = qsort (filter' (>x) xs)
    in ltSorted ++ [x] ++ gtSorted

largestDivisible :: (Integral a) => a
largestDivisible = head (filter' p [100000,99999..])
    where p x = x `mod` 3829 == 0

chain :: (Integral a) => a -> [a]
chain 1 = [1]
chain n
    | even n = n:chain (n `div` 2)
    | odd n  = n:chain (n*3 + 1)

numLongChains :: Int
numLongChains = length (filter isLong (map chain [1..100]))
    where isLong xs = length xs > 15

-- this version of numLongChains uses a lambda instead of 'isLong'
nLongChains' :: Int
nLongChains' = length (filter (\xs -> length xs > 15) (map chain [1..100]))

addThree :: (Num a) => a -> a -> a -> a
addThree x y z = x + y + z

-- an equivalent function, due to the way functions are curried by default
addThree' :: (Num a) => a -> a -> a -> a
addThree' = \x -> \y -> \z -> x + y + z

flips' :: (a -> b -> c) -> b -> a -> c
flips' f = \x y -> f y x

sum' :: (Num a) => [a] -> a
sum' = foldl (+) 0

elem' :: (Eq a) => a -> [a] -> Bool
elem' y ys = foldl (\acc x -> if x == y then True else acc) False ys

imap' :: (a -> b) -> [a] -> [b]
imap' f xs = foldr (\x acc -> f x : acc) [] xs

oddSquareSum :: Integer
oddSquareSum =
  let oddSquares = filter odd $ map (^2) [1..]
      belowLimit = takeWhile (<10000) oddSquares
  in  sum belowLimit
