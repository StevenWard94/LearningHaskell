-- vim: set foldlevel=1:
-- |
-- Module:        Edinburgh.Tutorial1
-- Author:        Steven Ward <stevenward94@gmail.com>
-- URL:           https://github.com/StevenWard94/LearningHaskell.d
-- Last Change:   2016 Aug 09
--

-- This module consists of my (attempted) solutions to the first Edinburgh Tutorial:
--                COMPREHENSION and RECURSION
--        Informatics 1 - Functional Programming: Tutorial 1
--
-- the instructions for which can be found at:
--      http://www.inf.ed.ac.uk/teaching/courses/inf1/fp/#haskell
--
--------------------------------------------------------------------------------
--
module Edinburgh.Tutorial1 where

import Test.QuickCheck
import Data.List ( (!!), isPrefixOf )
import Data.Char ( isDigit, digitToInt, toUpper, toLower )

-- Exercises 1-7    \begin1
-- Exercise 1    \begin2

-- 1. (a) Write a function, halveEvens :: [Int] -> [Int], that returns half of
-- each even number in the list. For example:
--                 halveEvens [0,2,1,7,8,56,17,18] == [0,1,428,9]
-- The function definition must use a LIST COMPREHENSION
halveEvens :: [Int] -> [Int]
halveEvens xs = [ x `div` 2 | x <- xs, x `mod` 2 == 0 ]

-- 1. (b) Write an equivalent function, halveEvensRec, using RECURSION
halveEvensRec :: [Int] -> [Int]
halveEvensRec [] = []
halveEvensRec [x] = if x `mod` 2 == 0 then [x `div` 2] else []
halveEvensRec (x:xs)
  | x `mod` 2 == 0 = (x `div` 2) : halveEvensRec xs
  | otherwise     = halveEvensRec xs

-- 1. (c) To confirm that the two functions are equivalent, write a test
-- function, prop_halveEvens, and run the appropriate QuickCheck test
prop_halveEvens :: [Int] -> Bool
prop_halveEvens xs = compFunc == recFunc
    where compFunc = halveEvens xs
          recFunc  = halveEvensRec xs

--               \end2

-- Exercise 2    \begin2

-- 2. (a) Write a function, inRange :: Int -> Int -> [Int] -> [Int], to return
-- all numbers in the input list that are within the specified range (inclusive)
-- using a LIST COMPREHENSION
inRange :: Int -> Int -> [Int] -> [Int]
inRange a b xs = [ x | x <- xs, x >= a && x <= b ]

-- 2. (b) Write an equivalent function, inRangeRec, using RECURSION
inRangeRec :: Int -> Int -> [Int] -> [Int]
inRangeRec _ _ []     = []
inRangeRec a b [x]    = if x >= a && x <= b then [x] else []
inRangeRec a b (x:xs) = action ++ inRangeRec a b xs
    where action = inRangeRec a b [x]

-- 2. (c) Write a test function, prop_inRange, to verify that the two functions
-- are equivalent
prop_inRange :: Int -> Int -> [Int] -> Bool
prop_inRange a b xs = comprehension == recursion
    where comprehension = inRange a b xs
          recursion     = inRangeRec a b xs

--               \end2

-- Exercise 3    \begin2

-- 3. (a) Write a function, countPositives, to count the positive numbers in
-- a list (excluding 0) using a LIST COMPREHENSION
countPositives :: [Int] -> Int
countPositives xs = length [ x | x <-xs, x > 0 ]

-- 3. (b) Write an equivalent function, countPositivesRec, using RECURSION
countPositivesRec :: [Int] -> Int
countPositivesRec list = case list of
                        []     -> 0
                        [x]    -> if x > 0 then 1 else 0
                        (x:xs) -> countPositivesRec [x] + countPositivesRec xs

-- 3. (c) Verify that the two functions are equivalent
prop_countPositives :: [Int] -> Bool
prop_countPositives xs = listComp == recursive
    where listComp  = countPositives xs
          recursive = countPositivesRec xs

--               \end2

-- Exercise 4    \begin2

-- 4. (a) Prof. Pennypincher won't buy anything over $199.99 but he gets a 10%
-- discount on anything he buys. Write a function, pennypincher, that takes
-- a list of prices and returns the total amount that Professor Pennypincher
-- would have to pay, assuming he bought everything that was cheap enough for
-- him. Prices should be in cents not dollars, by integers. To deduct 10% off
-- them, you will need to convert them to Float first, with the 'fromIntegral'
-- function. To convert back to Int, you can use the function 'round', which
-- rounds to the nearest integer. You can write a helper function to do this.
-- Your function definition should use a LIST COMPREHENSION
discount :: Int -> Int
discount fullPrice = round $ 0.9 * fromIntegral fullPrice

pennypincher :: [Int] -> Int
pennypincher prices = sum [ p | p <- map discount prices, p < 20000 ]

-- 4. (b) Write an equivalent function using RECURSION
pennypincherRec :: [Int] -> Int
pennypincherRec prices = case prices of
                           []     -> 0
                           [p]    -> if discount p < 200000 then discount p else 0
                           (p:ps) -> pennypincherRec [p] + pennypincherRec ps

-- 4. (c) Confirm that the two functions are equivalent
prop_pennypincher :: [Int] -> Bool
prop_pennypincher prices = pennypincher prices == pennypincherRec prices

--               \end2

-- Exercise 5    \begin2

-- 5. (a) Write a function, multDigits :: String -> Int, that returns the
-- product of all of the digits in the input string. If there are no digits, the
-- function should return 1. The function definition should use a LIST
-- COMPREHENSION
multDigits :: String -> Int
multDigits str = product [ digitToInt c | c <- str, isDigit c ]

-- 5. (b) Write an equivalent function multDigitsRec using RECURSION
multDigitsRec :: String -> Int
multDigitsRec str = case str of
                      []     -> 1
                      [s]    -> if isDigit s then digitToInt s else 1
                      (s:tr) -> multDigitsRec [s] * multDigitsRec tr

-- 5. (c) Verify that the two functions are equivalent
prop_multDigits :: String -> Bool
prop_multDigits str = multDigits str == multDigitsRec str

--               \end2

-- Exercise 6    \begin2

-- 6. (a) Write a function, capitalize :: String -> String, which, given a word,
-- capitalizes it, meaning that the first character should be made uppercase and
-- all other letters should be made lowercase. For Example, edINBurgh -> Edinburgh
-- The function definition should use a LIST COMPREHENSION and the library
-- functions, 'toUpper' and 'toLower'.
capitalize :: String -> String
capitalize [] = []
capitalize (w:ord) = toUpper w : [ toLower c | c <- ord ]

-- 6. (b) Write an equivalent function (and helper function, if necessary), capitalizeRec,
-- using RECURSION
capitalizeRec :: String -> String
capitalizeRec []     = []
capitalizeRec (w:ord) = toUpper w : lowerRec ord

lowerRec :: String -> String
lowerRec [] = []
lowerRec (w:ord) = toLower w : lowerRec ord

-- 6. (c) Write a test function, prop_capitalize, to confirm that the two
-- functions are equivalent
prop_capitalize :: String -> Bool
prop_capitalize word = capitalize word == capitalizeRec word

--               \end2

-- Exercise 7    \begin2

-- 7. (a) Using the capitalize function from Exercise 6, write a function,
-- title :: [String] -> [String], which, given a list of words, capitalizes only
-- the first letter of each word that is at least 4 letters long. THe function
-- definition should use a LIST COMPREHENSION

-- Auxilliary function that acts as a switch to determine proper capitalization
titleSwitch :: String -> String
titleSwitch word
    | length word > 3 = capitalize word
    | otherwise       = lowercase word

-- Another auxilliary function to make words shorter than 4 letters all lowercase
lowercase :: String -> String
lowercase word = [ toLower w | w <- word ]

-- Now the actual function...
title :: [String] -> [String]
title [] = []
title (w:words) = capitalize w : [ titleSwitch wrd | wrd <- words ]

-- 7. (b) Write an equivalent function, titleRec, using RECURSION
titleRecSwitch :: String -> String
titleRecSwitch word
    | length word > 3 = capitalizeRec word
    | otherwise       = lowerRec word

titleRec :: [String] -> [String]
titleRec [] = []
titleRec (w:words) = capitalizeRec w : recAuxFunc words
    where recAuxFunc []         = []
          recAuxFunc (wrd:wrds) = titleRecSwitch wrd : recAuxFunc wrds

-- 7. (c) Confirm that the two functions are equivalent with a test function
prop_title :: [String] -> Bool
prop_title words = title words == titleRec words

--               \end2
-- \end1

-- Optional Material    \begin1

-- Exercise 8    \begin2

-- 8. (a) Dame Curious has a long list of words that might appear in a crossword
-- puzzle but she has trouble finding the ones that fit a slot. Write
-- a function, crosswordFind, to help her. The expression,
--    crosswordFind letter inPosition len words, should return all items from
-- 'words' that are both of the given length AND have 'letter' in the position
-- 'inPosition'. For example, if Curious needs a 7-letter word with 'k' in
-- position 1, she can evaluate the expression:
--    crosswordFind 'k' 1 7 ["funky", "fabulous", "kite", "icky", "ukelele"],
-- which returns ["ukelele"]. The definition should use a LIST COMPREHENSION and
-- can use a library function to get the n-th element of a list and/or 'length'
crosswordFind :: Char -> Int -> Int -> [String] -> [String]
crosswordFind _ _ _ [] = []
crosswordFind letter inPosition len words = [ wrd | wrd <- words, wordCheck wrd ]
    where wordCheck word = and $ sequenceA [ ((== len) . length), ((== letter) . (!! inPosition)) ] word

-- 8. (b) Write an equivalent function using RECURSION
crosswordFindRec :: Char -> Int -> Int -> [String] -> [String]
crosswordFindRec _ _ _ [] = []
crosswordFindRec letter inPosition len (w:words)
    | wordCheck w = w : reapply words
    | otherwise   = reapply words
    where wordCheck word = and $ sequenceA [ ((== len) . length), ((== letter) . (!! inPosition)) ] word
          reapply = crosswordFindRec letter inPosition len

-- 8. (c) Write a QuickCheck property, prop_crosswordFind, to test the functions
-- for equivalency
prop_crosswordFind :: Char -> Int -> Int -> [String] -> Bool
prop_crosswordFind c pos len ws = listcomp == recursive
    where listcomp  = crosswordFind c (abs pos) len ws
          recursive = crosswordFindRec c (abs pos) len ws

--               \end2

-- Exercise 9    \begin2

-- 9. (a) Write a function, search, that returns the positions of all
-- occurrences of its second argument in the first. For example,
-- search "Bookshop" 'o' == [1,2,6]
-- search "senselessness's" 's' == [0,3,7,8,11,12,14]
-- The definition should use a LIST COMPREHENSION and may use the library
-- functions, 'zip' and 'length'
search :: String -> Char -> [Int]
search [] _ = []
search str c = [ i | i <- [0..length str - 1], str !! i == c ]

-- 9. (b) Write an equivalent function (and an auxiliarry function if needed)
-- called searchRec, which uses RECURSION
searchRec :: String -> Char -> [Int]
searchRec [] _ = []
searchRec str c = auxfunc str c 0
    where
        auxfunc [] _ _  = []
        auxfunc (s:tr) c i
            | s == c     = i : auxfunc tr c (i + 1)
            | otherwise = auxfunc tr c (i + 1)

--               \end2

-- Exercise 10   \begin2

-- 10. (a) Write a function, contains, that takes two strings and returns True
-- if the first string contains the second as a substring. The definition should
-- use a LIST COMPREHENSION and can use the library function, 'isPrefixOf'
contains :: String -> String -> Bool
contains _  []      = True
contains [] substr  = False
contains str substr = any (isPrefixOf substr) (slicer str)

slicer :: String -> [String]
slicer str = [ drop i str | i <- [0..length str]]

-- 10. (b) Write a function using RECURSION to the same specifications as
-- 'contains'
containsRec :: String -> String -> Bool
containsRec _ []       = True
containsRec str substr = case str of
                           []     -> False
                           _      -> if substr `isPrefixOf` str
                                        then True
                                        else containsRec (tail str) substr

-- test for equivalency with:
--    ghci> quickCheck (\str substr -> (contains str substr) == (containsRec str substr))

--               \end2
-- \end
