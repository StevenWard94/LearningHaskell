-- vim: set foldlevel=1:
-- |
-- Module:        Edinburgh.Tutorial1                              \begin
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
import Data.Char ( isDigit, digitToInt )

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
