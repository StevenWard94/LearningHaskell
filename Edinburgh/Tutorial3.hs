-- vim: set foldlevel=1:
-- |
-- Module:        Edinburgh.Tutorial3
-- Author:        Steven Ward <stevenward94@gmail.com>
-- URL:           https://github.com/StevenWard94/LearningHaskell.d
-- Last Change:   2016 Aug 15
--

-- This module consists of my (attempted) solutions to the third Edinburgh Tutorial:
--                      HIGHER-ORDER FUNCTIONS
--        Informatics 1 - Functional Programming: Tutorial 3
--
-- the instructions for which can be found at:
--      http://www.inf.ed.ac.uk/teaching/courses/inf1/fp/#haskell
--
--------------------------------------------------------------------------------
--
module Edinburgh.Tutorial3 where

import Test.QuickCheck
import Data.Char ( toUpper, isAlpha )
import Data.List ( transpose )

-- Map \begin

-- INTRODUCTION \begin2
-- Transforming every list element by a particular function is a common need
-- when processing lists — for example, we may want to...
--   –  add one to each element of a list of numbers
--   –  extract the first element of every pair in a list
--   –  convert every character in a string to uppercase
--   –  add a grey background to every picture in a list of pictures
-- The 'map' function captures this pattern, allowing us to avoid repititious
-- code that results from writing a recursive function for each case.
-- Consifer a function, g, defined in terms of an imaginary function f, as follows:
--     g []     = []
--     g (x:xs) = f x : g xs
-- This definition could also be written, equivalently, using 'map':
--     g xs = map f xs
-- Here is the definition of 'map':
--     map :: (a -> b) -> [a] -> [b]
--     map f []      = []
--     map f (x:xs)  = f x : map f xs
-- Given 'map' and a function that operates on a single element, we can easily
-- write a function that operatres on a list. For instance, the function that
-- extracts the first element of every pair can be defined as follows (using
-- fst :: (a,b) -> a):
--     fsts :: [(a,b)] -> [a]
--     fsts pairs = map fst pairs
--                                          \end2

-- Exercise 1    \begin2
-- Using map and other suitable library functions, write definitions for the
-- following...

-- 1. (a) A function, uppers, that converts a string to uppercase.
uppers :: String -> String
uppers str = map toUpper str

-- 1. (b) A function, doubles, that doubles every item in a list.
doubles :: [Int] -> [Int]
doubles xs = map (*2) xs

-- 1. (c) A function, penceToPounds, that turns prices given in pence into the
-- same price given in pounds
penceToPounds :: [Int] -> [Float]
penceToPounds = map $ (/100) . fromIntegral

-- 1. (d) Write a list-comprehension version of 'uppers' and use it to check the
-- answer to (a).
uppers' :: String -> String
uppers' str = [ toUpper ch | ch <- str ]

-- \end2
-- \end

-- Filter \begin

-- INTRODUCTION \begin2
-- Removing elements from a list is another common need. For example, we might
-- want to remove non-alphabetic characters from a string, or negative integers
-- from a list. This pattern is captured by the 'filter' function.
-- Here is the definition of 'filter':
--     filter :: (a -> Bool) -> [a] -> [a]
--     filter p []     = []
--     filter p (x:xs) | p x       = x : filter p xs
--                     | otherwise = filter p xs
-- \end2

-- Exercise 2 \begin2
-- Using 'filter' and other standard library functions, write definitions for
-- the following...

-- 2. (a) A function, alphas, that removes all non-alphabetic characters from
-- a string.
alphas :: String -> String
alphas str = filter isAlpha str

-- 2. (b) A function, rmChar, that removes all occurrences of a character from
-- a string.
rmChar :: Char -> String -> String
rmChar c = filter (/=c)

-- 2. (c) A function, above, that removes all numbers less than or equal to
-- a given number.
above :: (Num a, Ord a) => a -> [a] -> [a]
above n xs = filter (> n) xs

-- 2. (d) A function, unequals, that removes all pairs (x,y) where x == y
unequals :: (Num a, Eq a) => [(a,a)] -> [(a,a)]
unequals = filter $ uncurry (/=)

-- 2. (e) Write a list-comprehension version of 'rmChar' and use QuickCheck to
-- test it against the version using 'filter'
rmChar' :: Char -> String -> String
rmChar' x str = [ c | c <- str, c /= x ]

-- \end2
-- \end

-- Comprehensions, map and filter \begin

-- As we have seen, list comprehensions process a list using transformations
-- similar to 'map' and 'filter'. In general, [f x | x <- xs, p x] is equivalent
-- to, map f $ filter p xs

-- Exercise 3 \begin2
-- Write expressions equivalent to the following using 'map' and filter. Use
-- QuickCheck to verify your answers.

-- 3. (a) [toUpper c | c <- s, isAlpha c]
upperChars' :: String -> String
upperChars' s = [ toUpper c | c <- s, isAlpha c ]

upperChars :: String -> String
upperChars s = map toUpper $ filter isAlpha s

prop_upperChars :: String -> Bool
prop_upperChars s = upperChars' s == upperChars s


-- 3. (b) [2 * x | x <- xs, x > 3]
lgDoubles' :: [Int] -> [Int]
lgDoubles' xs = [ 2 * x | x <- xs, x > 3 ]

lgDoubles :: [Int] -> [Int]
lgDoubles xs = map (*2) $ filter (> 3) xs

prop_lgDoubles :: [Int] -> Bool
prop_lgDoubles xs = lgDoubles' xs == lgDoubles xs


-- 3. (c) [reverse s | s <-strs, even (length s)]
reverseEvenLen' :: [String] -> [String]
reverseEvenLen' strs = [ reverse s | s <- strs, even (length s) ]

reverseEvenLen :: [String] -> [String]
reverseEvenLen strs = map reverse $ filter (even . length) strs

prop_reverseEvenLen :: [String] -> Bool
prop_reverseEvenLen strs =
    reverseEvenLen' strs == reverseEvenLen strs

-- \end2
-- \end

-- Fold \begin

-- INTRODUCTION \begin2
-- The 'map' and 'filter' functions act on elements individually; they never
-- combine one element with another. Sometimes, it is necessary to combine
-- elements using some operation. For example, the 'sum' function can be written
-- like this:
--     sum []     = 0
--     sum (x:xs) = x + sum xs
-- Essentially, this just combines the elements of the given list using the '+'
-- operation. Another example is reverse:
--     reverse []     = []
--     reverse (x:xs) = reverse xs ++ [x]
-- This function just combines the elements, one by one, by appending them onto
-- the end of the reversed list. This time, the "combining" function is a little
-- harder to see. It may be more obvious written this way:
--     reverse []     = []
--     reverse (x:xs) = x `snoc` reverse xs
--
--     snoc x xs = xs ++ [x]
-- Now it's easier to see that `snoc` plays the same role as '+' in the example
-- of 'sum'. These, and many others, follow a pattern: breaking the list into
-- its head (x) and tail (xs), recursing on 'xs', and then applying some
-- function to 'x' and the modified 'xs'. The only things that must be specified
-- are the function (e.g. (+) or `snoc`) and the "initial value" (e.g. 0 in the
-- case of 'sum' or [] in the case of 'reverse'.
--
-- This pattern is called a "fold" and is implemented in Haskell via the
-- function, 'foldr'.
--                                  foldr :: (a -> b -> b) -> b -> [a] -> b
--     g []     = u                 foldr f u []     = u
--     g (x:xs) = x `f` g xs        foldr f u (x:xs) = x `f` foldr f u xs
-- The function, 'g', can be written with recursion (as above) or using a fold;
-- both definitions are equivalent:    g xs = foldr f u xs
--
-- One way to visualize the action of foldr is shown in the figure below. Given
-- a function, f :: a -> b -> b, an initial value, u :: b (sometimes called the
-- "unit"), and a list, [x₁,x₂,...,xₙ] of type [a]. the 'foldr' function returns
-- the value that results from replacing every cons (:) in 'list' with 'f' and
-- replacing the terminating [] (nil) with u.
--
--     x₁  :  (x₂  :  . . .  :  (xₙ  :  []  )...)
--         |       |         |       |   |
--         ↓       ↓         ↓       ↓   ↓
--     x₁ `f` (x₂ `f` . . . `f` (xₙ `f`  u  )...)
--
-- For example, 'sum :: [Int] -> Int' can be defined as follows, using (+) as
-- the function and 0 as the unit:
--     sum :: [Int] -> Int            10  :  20  :  30  :  []
--     sum ns = foldr (+) 0 ns            ↓      ↓      ↓   ↓
--                                    10  +  20  +  30  +   0
--                                illustration of 'foldr (+) 0 [10,20,30]'
-- \end2

-- Exercise 4 \begin2
-- To practice using 'foldr', we will write several functions, first with
-- recursion, and then using 'foldr'. For each pair of functions, use QuickCheck
-- to confirm that they are equivalent.

-- 4. (a) Write a recursive function, productRec, that computes the product of
-- the numbers in a list. Then, write an equivalent function, productFold, using
-- 'foldr'.
productRec :: [Int] -> Int
productRec []     = 1
productRec (x:xs) = x * productRec xs

productFold :: [Int] -> Int
productFold xs = foldr (*) 1 xs

prop_product :: [Int] -> Bool
prop_product xs = productRec xs == productFold xs


-- 4. (b) Write a recursive function, andRec, that checks whether every item in
-- a list is True. Then, write the same function using 'foldr', called andFold.
andRec :: [Bool] -> Bool
andRec []     = True
andRec (q:qs) = q && andRec qs

andFold :: [Bool] -> Bool
andFold qs = foldr (&&) True qs

prop_and :: [Bool] -> Bool
prop_and qs = andRec qs == andFold qs


-- 4. (c) Write a recursive function, concatRec, that concatenates a list of
-- lists into a single list. Then, write an equivalent function, concatFold,
-- using 'foldr'
concatRec :: [[a]] -> [a]
concatRec []     = []
concatRec (l:ls) = l ++ concatRec ls

concatFold :: [[a]] -> [a]
concatFold ls = foldr (++) [] ls

prop_concat :: Eq a => [[a]] -> Bool
prop_concat ls = concatRec ls == concatFold ls


-- 4. (d) Write a recursive function, rmCharsRec, that removes all characters in
-- the first string from the second string, using the 'rmChar' function from
-- exercise (2b). Then write the same function using 'foldr', rmCharsFold.
rmCharsRec :: String -> String -> String
rmCharsRec _  []      = []
rmCharsRec [] str     = str
rmCharsRec (c:cs) str = rmChar c $ rmCharsRec cs str

rmCharsFold :: String -> String -> String
rmCharsFold cs str = foldr rmChar str cs

prop_rmChars :: String -> String -> Bool
prop_rmChars str1 str2 = recursion == folding
    where recursion = rmCharsRec  str1 str2
          folding   = rmCharsFold str1 str2
-- \end2
-- \end

-- Matrix manipulation \begin

-- Next, we will look at matrix addition and multiplication. To represent
-- matrices, we will use lists of lists of Int values; for example:
--     ⌈ 1 4 9 ⌉                     [[1,4,9],
--     ⌊ 2 5 7 ⌋  is represented as   [2,5,7]] .
type Matrix = [[Int]]
-- Our first task is to write a test to show whether a given list of lists of Int
-- values is a matrix. This test should verify two things: 1) that the lists of
-- Int values are all of equal length, and 2) that there is at least one row and
-- one column in the list of lists.

-- Exercise 5 \begin2
-- 5. (a) Write a function, uniform, that tests whether the integers in a list
-- are all equal. You can use the library function, 'all', which tests whether
-- all elements of a list satisfy a given predicate. If you want, you can try to
-- define 'all' in terms of foldr and map.
all' :: (a -> Bool) -> [a] -> Bool
all' f = foldr (&&) True . map f

uniform :: [Int] -> Bool
uniform [] = True
uniform xs = all' (== head xs) (tail xs)

-- 5. (b) Using the function, 'uniform', write a function, valid, that tests
-- whether a list of lists of Int values is a matrix.
valid :: Matrix -> Bool
valid []     = False
valid (x:xs) = not (null x) && uniform (map length (x:xs))

-- \end2

-- A useful higher-order function is 'zipWith'. It is a lot like the function
-- 'zip', which takes two lists and combines the corresponding elements into
-- a list of pairs. The difference is that instead of combining elements as
-- a pair, zipWith uses a specified function to combine corresponding elements.
-- The definition (followed by an illustration) is as follows:
--     zipWith f [] _  = []
--     zipWith f _  [] = []
--     zipWith f (x:xs) (y:ys) = f x y : zipWith f xs ys
--
--       x₁     :   (x₂      : . . . :   (xₙ      :   []   )...)
--       y₁     :   (y₂      : . . . :   (yₙ      :   []   )...)
--       ↓           ↓                    ↓            ↓
--    f(x₁)(y₁) : (f(x₂)(y₂) : . . . : (f(xₙ)(yₙ) :   []   )...)
--
-- Another useful function for working with pairs is 'uncurry', which turns
-- a function that takes two arguments into a function that operates on a pair.

-- Exercise 6 \begin2
-- 6. (a) What is returned by the expression, uncurry (+) (10,8)?    Answer: 18

-- 6. (b) Define 'zipWith' using 'zip' and a list comprehension.
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' f xs ys = [ f x y | x <- xs, y <- ys ]

-- 6. (c) Define 'zipWith' using 'zip' and the higher-order functions, 'map' and
-- 'uncurry', instead of a list comprehension.
zipWith'' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith'' f xs ys = map (uncurry f) $ zip xs ys

-- \end2

-- Adding two matrices of equal size is done by pairwise-adding corresponding
-- elements (i.e. [a]ij + [b]ij = [c]ij) to form the corresponding element in
-- the sum. Now, we will use 'zipWith' to implement matrix addition.

-- Exercise 7 \begin2
-- Write a function, plusM, that adds two matrices. Return an error if the input
-- is not suitable for matrix addition. It may be helpful to define a helper
-- function, plusRow, that adds two rows of a matrix.
plusRow :: [Int] -> [Int] -> [Int]
plusRow = zipWith (+)

plusM :: Matrix -> Matrix -> Matrix
plusM mA mB =
        if invalid
            then undefined
            else zipWith plusRow mA mB
                where invalid = not . and $ [valid mA && valid mB, length mA == length mB, length (head mA) == length (head mB)]
-- \end2

-- For matrix multiplication, we are going to use the "dot product" or "inner
-- product" of two vectors, given by:
--         (a₁,a₂,...,aₙ) · (b₁,b₂,...,bₙ) ≡ a₁b₁ + a₂b₂ + ... + aₙbₙ
-- Matrix multiplication is then defined as follows: two matrices with
-- dimensions (n,m) and (m,p) are multiplied to form a matrix of dimension (n,p)
-- in which the element in row i, column j is the dot product of row i in the
-- first matrix and column j in the second. For example:
--         ⌈  1   10 ⌉   ⌈ 1 2 ⌉   ⌈  31  42 ⌉
--         ⌊ 100  10 ⌋ × ⌊ 3 4 ⌋ ⁼ ⌊ 130 240 ⌋

-- Exercise 8 \begin2
-- Define a function, timesM, to perform matrix multiplication. Return an error
-- if the input is not suitable. It may be helpful to define a helper function,
-- dot, for the dot product of two vectors. The function should take the dot
-- product of the single row with every column of the matrix, and return the
-- values as a list. To make the columns of a matrix readily available, you can
-- use the function 'transpose'.
type Vec = [Int]

nRows :: Matrix -> Int
nRows = length

nCols :: Matrix -> Int
nCols = length . head

dot :: Vec -> Vec -> Int
dot u v = foldr (+) 0 $ zipWith (*) u v

timesM :: Matrix -> Matrix -> Matrix
timesM mA mB
  | not invalid = [ [ dot rowV colV | colV <- transpose mB ] | rowV <- mA ]
  | otherwise = undefined
  where invalid = not . and $ [ valid mA, valid mB, nCols mA == nRows mB ]

-- \end2
-- \end

-- Optional Material \begin

-- For a real challenge, you can try to compute the inverse of a matrix. There
-- are a few steps involved in this process:
--   (a) The entries of the matrix should be changed to Double or (even better)
--     Rational values to allow proper division.
--
--   (b) You will need a function to find the "determinant" of a matrix. This
--     will tell you if it has an inverse.
--
--   (c) You will need a function to do the actual inversion.
--
-- There are several different algorithms available to compute the determinant
-- and the inverse of a matrix. Good places to start looking are:
--     http://mathworld.wolfram.com/MatrixInverse.html
--     http://en.wikipedia.org/wiki/Invertible_matrix
--
-- Finally, implement an appropriate quickCheck test for your function.

type Matrix_ a = [[a]]

height :: Matrix_ a -> Int
height = length

width :: Matrix_ a -> Int
width = length . head

valid_ :: Matrix_ a -> Bool
valid_ [] = False
valid_ m  = not (null . head $ m) && uniform (map length m)

(.+) :: (Num a) => Matrix_ a -> Matrix_ a -> Matrix_ a
mA .+ mB
  | sumExists = zipWith (zipWith (+)) mA mB
  | otherwise = undefined
  where sumExists = and [ valid_ mA, valid_ mB, width mA == width mB, height mA == height mB ]
infixl 6 .+

type Vector a = [a]
(*.) :: (Num a) => Vector a -> Vector a -> a
u *. v = foldr (+) 0 $ zipWith (*) u v
infixl 7 *.

(.*) :: (Num a) => Matrix_ a -> Matrix_ a -> Matrix_ a
mA .* mB
  | dotExists = [ [ rowV *. colV | colV <- transpose mB ] | rowV <- mA ]
  | otherwise = undefined
  where dotExists = and [ valid_ mA, valid_ mB, width mA == height mB ]
infixl 7 .*

matrixMap :: (a -> b) -> Matrix_ a -> Matrix_ b
matrixMap = map . map

matrixZipWith :: (a -> b -> c) -> Matrix_ a -> Matrix_ b -> Matrix_ c
matrixZipWith = zipWith . zipWith

rmPermute :: [a] -> Matrix_ a
rmPermute xs = case xs of
                 []     -> []
                 (x:xs) -> xs : map (x :) (rmPermute xs)

-- this function takes a matrix and returns a matrix of matrices resulting from the
-- "permuted removal" of one row and one column from the original matrix (this simulates
-- the method of using "minors" to calculate det() of large matrices)
minorsMatrix :: Matrix_ a -> Matrix_ (Matrix_ a)
minorsMatrix = map (map transpose . rmPermute . transpose) . rmPermute

-- this function generates a permutation matrix for the given height (h) and
-- width (w)
perMatrix :: (Num a) => Int -> Int -> Matrix_ a
perMatrix w h = take h $ cycle [ iEven, iOdd ]
    where iEven = take w $ cycle [1,-1]
          iOdd  = take w $ cycle [-1,1]

det :: (Num a) => Matrix_ a -> a
det [[a]] = a
det m     = sum $ zipWith (*) (head $ matrixZipWith f m (minorsMatrix m)) (cycle [1,-1])
    where f a m = a * det m

cofactors :: (Num a) => Matrix_ a -> Matrix_ a
cofactors m = matrixZipWith (*) (matrixMap det $ minorsMatrix m) sgn
    where sgn = perMatrix (width m) (height m)

expand :: (Num a) => a -> Matrix_ a -> Matrix_ a
expand = map . map . (*)

invert :: (Fractional a) => Matrix_ a -> Matrix_ a
invert m = expand (1 / det m) (transpose $ cofactors m)


-- Test Functions
identity :: (Num a) => Int -> Matrix_ a
identity r = map replrep [0..r-1]
    where replrep m = take r $ replicate m 0 ++ [1] ++ repeat 0
