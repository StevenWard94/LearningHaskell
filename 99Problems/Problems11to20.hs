-- |
-- Module:        Probles11to20
-- Author:        Steven Ward <stevenward94@gmail.com>
-- URL:           https://github.com/StevenWard94/LearningHaskell.d
-- Last Change:   2016 Sep 15
--

module Problems11to20 where

import Data.List ( length, group )

import GHC.Exts ( build )

-- Problem 11: modified run-length encoding \begin
-- (details) Modify the result of Problem 10 so that elements without duplicates
-- are simply copied into the resulting list instead of being transferred as (N E)
-- lists. Example:
--     *Main> encodeModified "aaaabccaadeeee"
--     [Multiple 4 'a', Single 'b', Multiple 2 'c', Multiple 2 'a', Single 'd',
--     Multiple 4 'e']

-- a copy of 'myEncode' from Problem 10
encode :: (Eq a) => [a] -> [(Int,a)]
encode = map (\xs -> (length xs, head xs)) . group

data Element a = Single a | Multiple Int a
    deriving (Show)

modifiedEncode :: (Eq a) => [a] -> [Element a]
modifiedEncode = map auxEncode . encode
    where
        auxEncode (1,x) = Single x
        auxEncode (n,x) = Multiple n x

-- could also be solved using a list comprehension like so:
encodeModified :: (Eq a) => [a] -> [Element a]
encodeModified xs = [ y | x <- group xs,
                     let y = if (length x) == 1 then Single (head x) else Multiple (length x) (head x) ]

-- \end

-- Problem 12: decode a run-length encoded list \begin
-- (details) Given a run-length encoded list (as specified in Problem 11),
-- constuct its uncompressed (original) version. For example:
--     *Main> decodeModified
--              [Multiple 4 'a', Single 'b', Multiple 2 'c',
--               Multiple 2 'a', Single 'd', Multiple 4 'e']
--     "aaaabccaadeeee"
decodeModified :: [Element a] -> [a]
decodeModified = concatMap decodeAux
    where
        decodeAux (Single x)     = [x]
        decodeAux (Multiple n x) = replicate n x

-- this can be done for the simpler encoding in Problem 10 like so:
decode :: [(Int,a)] -> [a]
decode = concatMap (uncurry replicate)

-- alternative using helper function called 'toTuple'
toTuple :: Element a -> (Int,a)
toTuple e = case e of
              (Single x)     -> (1, x)
              (Multiple n x) -> (n, x)

decodeMod :: [Element a] -> [a]
decodeMod = concatMap (uncurry replicate . toTuple)


-- solutions using 'foldl'
foldlDecode :: [Element a] -> [a]
foldlDecode = foldl (\x y -> x ++ helperF y) []
    where
        helperF (Single x)     = [x]
        helperF (Multiple n x) = replicate n x

foldlDecode' :: [Element a] -> [a]
foldlDecode' = foldl ( \acc e ->
        case e of
            Single x -> acc ++ [x]
            Multiple n x -> acc ++ replicate n x ) []

-- another way to decode the simplified encoding (Problem 10 style)
decode' :: (Eq a) => [(Int,a)] -> [a]
decode' = foldr f ([])
    where
        f (1, x) r = x : r
        f (k, x) r = x : f (k-1, x) r

-- or, to make it a good transformer for list fusion, like so:
{-# INLINE decode #-}
decode'' :: (Eq a) => [(Int,a)] -> [a]
decode'' xs = build (\c n ->
    let
      f (1, x) r = x `c` r
      f (k, x) r = x `c` f (k-1, x) r
    in
      foldr f n xs  )

-- \end
