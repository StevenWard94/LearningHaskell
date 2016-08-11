-- vim: set fdm=expr:
-- |
-- Module:        Edinburgh.Tutorial2
-- Author:        Steven Ward <stevenward94@gmail.com>
-- URL:           https://github.com/StevenWard94/LearningHaskell.d
-- Last Change:   2016 Aug 10
--

-- This module consists of my (attempted) solutions to the second Edinburgh Tutorial:
--                       THE CAESAR CIPHER
--        Informatics 1 - Functional Programming: Tutorial 2
--
-- the instructions for which can be found at:
--      http://www.inf.ed.ac.uk/teaching/courses/inf1/fp/#haskell
--
--------------------------------------------------------------------------------
--
module Edinburgh.Tutorial2 where

import Data.Char ( chr, isAlpha, isDigit, isUpper, isLower, toUpper, ord )
import Data.List ( isPrefixOf )
import Test.QuickCheck

rotate :: Int -> [Char] -> [Char]
rotate n cs
    | n >= 0 && n <= length cs = drop n cs ++ take n cs
    | n < 0                 = error "rotation amount cannot be less than 0"
    | otherwise             = error $ "rotation amount cannot be greater than the number of characters: " ++ show (length cs)

prop_rotate :: Int -> [Char] -> Bool
prop_rotate n cs = rotate (l - m) (rotate m cs) == cs
    where l = length cs
          m = if l == 0 then 0 else n `mod` l

makeKey :: Int -> [(Char,Char)]
makeKey n = keyZip alphabet (rotate n alphabet)
    where alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
          keyZip [] []         = []
          keyZip (c:cs) (d:ds) = (c,d) : keyZip cs ds
