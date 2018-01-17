-- |
-- Module:        Edinburgh.Tutorial2
-- Author:        Steven Ward <stevenward94@gmail.com>
-- URL:           https://github.com/StevenWard94/LearningHaskell.d
-- Last Change:   2016 Aug 11
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
import Data.List ( isPrefixOf, transpose )
import Test.QuickCheck

--------------------------------------------------------------------------------
--                            ENCRYPTING TEXT                                 --
--------------------------------------------------------------------------------

-- | Exercise 1
rotate :: Int -> [Char] -> [Char]
rotate n cs
    | n >= 0 && n <= length cs = drop n cs ++ take n cs
    | n < 0                 = error "rotation amount cannot be less than 0"
    | otherwise             = error $ "rotation amount cannot be greater than the number of characters: " ++ show (length cs)

-- | Exercise 2
prop_rotate :: Int -> [Char] -> Bool
prop_rotate n cs = rotate (l - m) (rotate m cs) == cs
    where l = length cs
          m = if l == 0 then 0 else n `mod` l

-- | Exercise 3
alphabet :: [Char]
alphabet = ['A'..'Z']

makeKey :: Int -> [(Char,Char)]
makeKey n = keyZip alphabet (rotate n alphabet)
    where keyZip [] []         = []
          keyZip (c:cs) (d:ds) = (c,d) : keyZip cs ds

-- | Exercise 4
-- Write a function, lookUp, that finds a pair by its FIRST component and
-- return's that pair's SECOND component. If the function is called with
-- a character that does not occur in the cipher key, it should return that
-- character unchanged. Use LIST COMPREHENSION, then write an equivalent
-- function, lookUpRec, using recursion. Test that the two functions are
-- equivalent with a test function, prop_lookUp
lookUp :: Char -> [(Char,Char)] -> Char
lookUp c key = head $ [ b | (a,b) <- key, a == c ] ++ [c]

lookUpRec :: Char -> [(Char,Char)] -> Char
lookUpRec c key = case key of
                    ((a,b):asbs) -> if a == c then b else lookUpRec c asbs
                    []           -> c

prop_lookUp :: Char -> [(Char,Char)] -> Bool
prop_lookUp c key = listcomp == recursion
    where listcomp  = lookUp c key
          recursion = lookUpRec c key

-- | Exercise 5
-- Write a function, encipher, that encrypts a given single character using
-- a key generated from a given offset. For example:
--     Main> encipher 5 'C'
--     'H'
--     Main> encipher 7 'Q'
--     'X'
encipher :: Int -> Char -> Char
encipher off ch = lookUp ch $ makeKey off

-- | Exercise 6
-- Text encrypted by a cipher is conventionally written in uppercase without
-- punctuation. Write a function, normalize, that converts a string to
-- uppercase, removing all non-alphanumeric characters. Example:
--     Main> normalize "July 4th!"
--     "JULY4TH"
normalize :: String -> String
normalize abnormal = [ toUpper ch | ch <- abnormal, isAlnum ch ]
    where isAlnum = \c -> or $ sequenceA [isAlpha, isDigit] c

-- | Exercise 7
-- Write a function, encipherStr, that normalizes a string and encrypts it,
-- using the functions 'normalize' and 'encipher' from above. Example:
--     Main> encipherStr 5 "July 4th!"
--     "OZQD4YM"
encipherStr :: Int -> String -> String
encipherStr n str = [ encipher n c | c <- normalize str ]



--------------------------------------------------------------------------------
--                           DECODING A MESSAGE                               --
--------------------------------------------------------------------------------

-- | Exercise 8
-- Decrypting an encoded message is easiest if first, the key is transformed.
-- Write 2 functions, reverseKey and reverseKeyRec, that swap each pair in
-- a given list, first using a LIST COMPREHENSION and then using RECURSION.
-- For example:
--     Main> reverseKey [('A','G'),('B','H'),('C','I')]
--     [('G','A'),('H','B'),('I,'C')]
-- Then, verify that the two functions are equivalent with a test function, prop_reverseKey.
reverseKey :: [(Char,Char)] -> [(Char,Char)]
reverseKey key = [ (j,i) | (i,j) <- key ]

reverseKeyRec :: [(Char,Char)] -> [(Char,Char)]
reverseKeyRec key = case key of
                      []           -> []
                      ((k,v):ksvs) -> (v,k) : reverseKeyRec ksvs

prop_reverseKey :: [(Char,Char)] -> Bool
prop_reverseKey key = reverseKey key == reverseKeyRec key

-- | Exercise 9
-- Write two functions. decipher and decipherStr, that decipher a character and
-- a string, respectively, using a key generated from the given offset. The
-- functions should leave digits and spaces unchanged, but remove lowercase
-- letters and other (i.e. non-alphanumeric or space) characters. Example:
--     Main> decipherStr 5 "OZQD4YM"
--     "JULY4TH"
decipher :: Int -> Char -> Char
decipher n c
    | isNumSpace c = c
    | isUpper c    = lookUp c $ reverseKey . makeKey $ n
    | otherwise    = '_'
    where isNumSpace c = or $ sequenceA [isDigit,((== 32) . ord)] c

decipherStr :: Int -> String -> String
decipherStr n str = filter (/= '_') [ decipher n ch | ch <- str ]



--------------------------------------------------------------------------------
--                         BREAKING THE ENCRYPTION                            --
--------------------------------------------------------------------------------

-- | Exercise 10
-- Write a function, contains, that returns True if the first string contains
-- the second as a substring (this exercise is the same as the last of the
-- optional exercises in Tutorial1)
contains :: String -> String -> Bool
contains _   []     = True
contains str substr = any (isPrefixOf substr) [ drop i str | i <- [0..length str] ]

-- | Exercise 11
-- Write a function, candidates, that decrypts the input string with each of the
-- 26 possible keys and, when the decrypted text contains "THE" or "AND",
-- includes the decryption key and the text in the output list.
candidates :: String -> [(Int,String)]
candidates str = [ (key, decipherStr key str) | key <- [1..25], isCandidate . decipherStr key $ str ]
    where isCandidate = \str -> str `contains` "THE" || str `contains` "AND"



--------------------------------------------------------------------------------
--                            OPTIONAL MATERIAL                               --
--------------------------------------------------------------------------------

-- | Exercise 12
-- Write a function, splitEachFive, that splits a string into substrings of
-- length 5. Pad the final substring with copies of 'X' if it is too short.
-- For example:    Main> splitEachFive "Secret Message"
--                 ["Secre", "t Mes", "sageX"]
splitEachFive :: String -> [String]
splitEachFive str
    | length str > 5 = take 5 str : splitEachFive (drop 5 str)
    | otherwise      = [padLenX str]
    where padLenX = \str -> take 5 $ str ++ repeat 'X'

-- | Exercise 13
-- The library function, transpose, switches the rows and columns of a list of
-- lists:
--     transpose ["123", "abc", "ABC"] == ["1aA", "2bB", "3cC"]
--     transpost ["1", "22", "333"] == ["123", "23", "3"]
-- If the rows in a list are all of the same length, transposing it twice
-- returns the original list. Use the splitEachFive function to write
-- a quickCheck property to test this.
prop_transpose :: String -> Bool
prop_transpose str = let splitStr = splitEachFive str
                      in splitStr == (transpose $ transpose splitStr)

-- | Exercise 14
-- Write a function, encrypt, that encrypts a string by first applying the
-- Caesar Cipher, then splitting it into pieces of length 5, transposing, and
-- putting the pieces together as a single string
encrypt :: Int -> String -> String
encrypt n str = concat $ transpose . splitEachFive $ encipherStr n str

-- | Exercise 15
-- Write a function to decrypt messages that have been encrypted in the way use
-- above.
fiveSplit :: String -> [String]
fiveSplit str
    | length str `mod` 5 == 0 = splitEach (length str `div` 5) str
    | otherwise              = undefined
    where splitEach _ [] = []
          splitEach n str = take n str : splitEach n (drop n str)

decrypt :: Int -> String -> String
decrypt n str = concat $ transpose . fiveSplit $ decipherStr n str
