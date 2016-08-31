module Problems1to10 where

import Control.Applicative ( (<*>) )
import Control.Arrow ( (&&&) )
import Control.Monad ( liftM2 )
import Data.List ( foldl' )

-- Problem 1: find the last element of a list \begin
myLast :: [a] -> a
myLast = head . reverse

-- alternatives...
myLast' l = case l of
              []     -> error "Empty list has no 'last element'"
              [x]    -> x
              (_:xs) -> myLast' xs

myLast'', myLast''', myLast'''' :: [a] -> a
myLast''   = foldr1 (const id)
myLast'''  = foldr1 (flip const)
myLast'''' = foldr1 (curry snd)

myLast''''' l
  | null l    = error "Empty list has no 'last element'"
  | otherwise = l !! length l - 1

-- \end

-- Problem 2: find the second to last element of a list \begin
myButLast :: [a] -> a
myButLast l = case l of
                []  -> error "No second to last of empty list"
                [x] -> error "No second to last of single element"
                _   -> head . reverse $ take (length l - 1) l

-- alternatives...
butLast, butLast', butLast'', butLast''' :: [a] -> a
butLast = last . init

butLast' l = reverse l !! 1

butLast'' [x,_]  = x
butLast'' (_:xs) = butLast'' xs

butLast''' = head . tail . reverse

lastbut1 :: (Foldable t) => t a -> a
lastbut1 = fst . foldl (\(a,b) x -> (b,x)) (err1, err2)
    where
        err1 = error "lastbut1: Empty list"
        err2 = error "lastbut1: Singleton"

lastbut1safe :: (Foldable t) => t a -> Maybe a
lastbut1safe = fst . foldl (\(a,b) x -> (b,Just x)) (Nothing,Nothing)

-- \end

-- Problem 3: find the k'th element of a list (index begins at 1) \begin
boundErr :: t
boundErr = error "Index out of bounds"

myElemAt :: [a] -> Int -> a
myElemAt l k
  | k < 1 || length l < k  =  boundErr
  | otherwise             =  fst . last $ zip l [1..k]

-- alternatives...
elemAt l k       =  l !! k - 1

elemAt' (x:_) 1  =  x
elemAt' []    _  =  boundErr
elemAt' (_:xs) k
  | k < 1        =  boundErr
  | otherwise    =  elemAt' xs (k - 1)

elemAt'' ls k    =  head $ foldr ($) ls
                         $ replicate (k - 1) tail
-- note: elemAt'' does not handle negative indices correctly...
--     Main> elemAt'' "haskell" (-1)
--     'h'

elemAt''' ls k
  | length ls < k = boundErr
  | otherwise     = last $ take k ls    -- equivalent to 'head . reverse $ take k ls'

elemAt'''' ls k
  | length ls < k = boundErr
  | otherwise     = head $ drop (k - 1) ls

elemAtPtFree :: [a] -> Int -> a
elemAtPtFree     = flip $ (last .) . take . (+1)

-- \end

-- Problem 4: find the number of elements in a list \begin
myLength :: [a] -> Int
myLength  = foldr (const (+1)) 0

myLength' :: [a] -> Int
myLength' = sum . map (\_ -> 1)

-- alternatives...
simpleLen ls = len_acc ls 0
    where
               len_acc [] n = n
               len_acc (_:xs) n = len_acc xs (n + 1)

foldLength1, foldLength2, foldLength3, foldLength4, foldLength5 :: [a] -> Int
foldLength1   =  foldl' (\acc _ -> acc + 1) 0
foldLength2   =  foldr (\_ acc -> acc + 1) 0
foldLength3   =  foldr (\_ -> (+1)) 0
foldLength4   =  foldr ((+) . (const 1)) 0
foldLength5   =  foldl' (const . (+1)) 0

zipLength1, zipLength2, zipLength3 :: [a] -> Int
zipLength1 xs =  snd $ last $ zip xs [1..]
zipLength2    =  snd . last . (flip zip [1..])
zipLength3    =  fst . last . zip [1..]

-- \end

-- Problem 5: reverse a list \begin
myReverse :: [a] -> [a]
myReverse  =  foldr (:) []

-- alternatives...

simpleReverse :: [a] -> [a]
simpleReverse []      =  []
simpleReverse (x:xs)  =  simpleReverse xs ++ [x]
-- this definition is more wasteful than the Prelude's definition as it
-- repeatedly "re-conses" (i.e. reconstructs) the result as it is accumulated.
-- The following version avoids this...
betterReverse :: [a] -> [a]
betterReverse list    =  reverse' list []
    where
        reverse' [] reversed     = reversed
        reverse' (x:xs) reversed = reverse' xs (x:reversed)

complexReverse :: [a] -> [a]
complexReverse xs     =  foldr (\x fId empty -> fId (x : empty)) id xs []

-- \end

-- Problem 6: find out whether a list is a palindrome \begin
myPalindromeChk  :: (Eq a) => [a] -> Bool
myPalindromeChk       =  liftM2 (==) id reverse

myPalindromeChk' :: (Eq a) => [a] -> Bool
myPalindromeChk' []   =  True
myPalindromeChk' [_]  =  True
myPalindromeChk' xs   =  head xs == last xs && (myPalindromeChk' $ init $ tail xs)

-- alternatives...
isPalindrome1, isPalindrome2, isPalindrome3, isPalindrome4, isPalindrome5, isPalindrome6 :: (Eq a) => [a] -> Bool

isPalindrome1 xs  =  foldl' (\acc (a,b) -> if a == b then acc else False) True input
    where
        input = zip xs $ reverse xs

isPalindrome2     =  (==) <*> reverse

isPalindrome3 xs  =  p [] xs xs
    where p rev (x:xs) (_:_:ys)  =  p (x:rev) xs ys
          p rev (x:xs) [_]       =  rev == xs
          p rev xs     []        =  rev == xs

isPalindrome4 xs  =  and $ zipWith (==) xs $ reverse xs

isPalindrome5 ls  =  fs_prt == reverse sn_prt
    where
        len                =  length ls
        half_len           =  len `div` 2
        (fs_prt, sn_prt')  =  splitAt half_len ls
        sn_prt             =  drop (len `mod` 2) sn_prt'

isPalindrome6     =  (uncurry (==) . (id &&& reverse))

-- \end
