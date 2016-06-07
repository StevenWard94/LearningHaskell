{- | A Haskell script for practicing writing definitions of math functions
 - Module:        Learn.Math
 - Author:        Steven Ward <stevenward94@gmail.com>
 - URL:           https://github.com/StevenWard94/LearningHaskell
 - Last Change:   2016 June 07
 -}

bin2dec :: Integral a => a -> a
bin2dec x
    | x < 0 = error "number of bits cannot be negative"
    | otherwise = 2 ^ x

compareToHundred :: (Num a, Ord a) => a -> Ordering
compareToHundred = compare 100

divideByTen :: (Floating a) => a -> a
divideByTen = (/10)

isUpperAlpha :: Char -> Bool
isUpperAlpha = (`elem` ['A'..'Z'])
