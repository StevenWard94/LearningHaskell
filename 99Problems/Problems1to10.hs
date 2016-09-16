-- |
-- Module:        Problems1to10
-- Author:        Steven Ward <stevenward94@gmail.com>
-- URL:           https://github.com/StevenWard94/LearningHaskell.d
-- Last Change:   2016 Sep 15
--
module Problems1to10 where

import Control.Applicative ( (<*>) )
import Control.Arrow ( (&&&) )
import Control.Monad ( liftM2 )
import Data.List ( findIndex, foldl', group )

import GHC.Exts ( build )

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

-- Problem 7: flatten a nested list structure \begin
-- (details) Transform a list, possibly holding lists as elements, into a "flat"
-- list by replacing each list with its elements, recursively.
--
-- Requires definition of a new data type b/c lists are homogenous in Haskell.
data NestedList a = Elem a | List [NestedList a]

myFlatten :: NestedList a -> [a]
myFlatten (Elem x ) = [x]
myFlatten (List xs) = foldr (++) [] $ map myFlatten xs

-- alternatives...
flatten1, flatten2, flatten3, flatten4, flatten5, flatten6 :: NestedList a -> [a]

flatten1 (Elem x) = [x]
flatten1 (List l) = concatMap flatten1 l

-- ...or without concatMap...
flatten2 (Elem a     ) = [a]
flatten2 (List (x:xs)) = flatten2 x ++ flatten2 (List xs)
flatten2 (List [])     = []

-- ...or using things that act just like concatMap...
flatten3 (Elem x) = return x
flatten3 (List l) = flatten3 =<< l

flatten4 (Elem x) = [x]
flatten4 (List l) = foldMap flatten4 l

flatten5 a = flt' a []
    where flt' (Elem x     ) xs = x:xs
          flt' (List (l:ls)) xs = flt' l $ flt' (List ls) xs
          flt' (List []    ) xs = xs

-- ...or with an accumulator function...
flatten6 = reverse . rec []
    where
        rec acc (List [])     = acc
        rec acc (Elem x)      = x:acc
        rec acc (List (x:xs)) = rec (rec acc x) (List xs)
-- \end

-- Problem 8: eliminate consecutive duplicates of list elements \begin
-- (details) If a list contains repeated elements, they should be replaced by
-- a single copy of the element. The order of elements should not be changed.
-- Example:
--     *Main> compress "aaaabccaadeeee"
--     "abcade"

myCompress :: (Eq a) => [a] -> [a]
myCompress l = foldr (\a b -> if a == head b then b else a:b) [last l] l

myCompress' :: (Eq a) => [a] -> [a]
myCompress' = map head . group

-- alternatives...
compress1 (x:ys@(y:_))
  | x == y    = compress1 ys
  | otherwise = x : compress1 ys
compress1 ys  = ys

compress2 xs = foldr f (const []) xs Nothing
    where
        f x r a@(Just q) | x == q = r a
        f x r _ = x : r (Just x)

compress3 :: (Eq a) => [a] -> [a]
compress3 = foldr skipDubs []
    where skipDubs x [] = [x]
          skipDubs x acc
            | x == head acc = acc
            | otherwise    = x : acc

compress4 list = compress_acc list []
    where
        compress_acc []  acc = acc
        compress_acc [x] acc = (acc ++ [x])
        compress_acc (x:xs) acc
          | x == head xs      = compress_acc xs acc
          | otherwise        = compress_acc xs (acc ++ [x])

compress5 []     = []
compress5 (x:xs) = x : (compress5 $ dropWhile (== x) xs)

compress6 x = reverse $ foldl' (\a b -> if head a == b then a else b:a) [head x] x

{-# INLINE compress7 #-}
compress7 :: (Eq a) => [a] -> [a]
compress7 xs = build (\c n ->
    let
      f x r a@(Just q) | x == q = r a
      f x r _ = x `c` r (Just x)
    in
      foldr f (const n) xs Nothing
                     )

-- \end

-- Problem 9: pack consecutive duplicates of list elements into sublists \begin
-- (details) If a list contains repeated elements, they should be placed in
-- separate sublists. Example:
--     *Main> pack ['a','a','a','a','b','c','c','a','a','d','e','e','e','e']
--     ["aaaa","b","cc","aa","d","eeee"]

myPack :: (Eq a) => [a] -> [[a]]
myPack = foldr f []
    where
        f x []     = [[x]]
        f x (y:xs) =
            if x == head y then ((x:y):xs) else ([x]:y:xs)

-- alternatives...
pack1 (x:xs) = let (first,rest) = span (== x) xs
               in (x:first) : pack1 rest
pack1 []     = []

pack2 []     = []
pack2 (x:xs) = (x:first) : pack2 rest
    where
        getReps [] = ([],[])
        getReps (y:ys)
          | y == x     = let (f,r) = getReps ys in (y:f, r)
          | otherwise = ([], (y:ys))
        (first,rest) = getReps xs

pack3 []     = []
pack3 (x:xs) = (x:reps) : (pack3 rest)
    where
        (reps,rest) = maybe (xs,[]) (\i -> splitAt i xs) (findIndex (/= x) xs)

pack4 []     = []
pack4 (x:xs) = (x : takeWhile (== x) xs) : pack4 (dropWhile (== x) xs)

pack5 []     = []
pack5 [x]    = [[x]]
pack5 (x:xs) = if x `elem` head (pack5 xs)
                  then (x : head (pack5 xs)) : tail (pack5 xs)
                  else [x] : pack5 xs

pack6 []  = []
pack6 [x] = [[x]]
pack6 (x:xs)
  | x == head h_p_xs = (x:h_p_xs):t_p_hs
  | otherwise       = [x]:p_xs
  where p_xs@(h_p_xs:t_p_hs) = pack6 xs

pack7 []     = []
pack7 (y:ys) = impl ys [[y]]
    where
               impl [] packed = packed
               impl (x:xs) packed
                 | x == (head . last) packed = impl xs $ (init packed) ++ [x:(last packed)]
                 | otherwise                = impl xs $ packed ++ [[x]]

pack8 []     = []
pack8 (y:ys) = reverse $ impl ys [[y]]
    where
        impl [] packed = packed
        impl (x:xs) p@(z:zs)
          | x == head z = impl xs $ (x:z):zs
          | otherwise  = impl xs $ [x]:p

-- \end

-- Problem 10: run-length encoding of a list \begin
-- (details) Use the result of Problem 9 to implement the so-called "run-length
-- encoding" data compression method. Consecutive duplicates of elements are
-- encoded as lists, (N E), where N is the number of duplicates of element E.
-- Example:
--     *Main> encode "aaaabccaadeeee"
--     [(4,'a'), (1,'b'), (2,'c'), (2,'a'), (1,'d'), (4,'e')]
myEncode :: (Eq a) => [a] -> [(Int,a)]
myEncode = map (\xs -> (length xs, head xs)) . group

myEncode' :: (Eq a) => [a] -> [(Int,a)]
myEncode' []     = []
myEncode' (x:xs) = (length $ x : takeWhile (==x) xs, x)
                    : myEncode' (dropWhile (==x) xs)

-- alternatives...
-- using the '&&&' arrow operator for tuples
encode1 xs = map (length &&& head) $ group xs

-- slightly more verbose w/ applicative combinators instead of '&&&'
encode2 :: Eq a => [a] -> [(Int,a)]
encode2 = map ((,) <$> length <*> head) . myPack

-- with 'foldr'
encode3 xs = (enc . myPack) xs
    where enc = foldr (\x acc -> (length x, head x) : acc) []

-- without higher-order functions
encode4 []     = []
encode4 (x:xs) = enc 1 x xs
    where
        enc n x []    = [(n,x)]
        enc n x (y:ys)
          | x == y     = enc (n+1) x ys
          | otherwise = (n,x) : enc 1 y ys

-- with 'zip' and 'group'
encode5 xs =
    let l = group xs
        h = map head l
     in zip (map length l) h

-- or, ignoring the rule that we should use the result of Problem 9
encode6 xs = foldr f final xs Nothing
    where
      f x r (Just a@(i,q))
        | x == q         = r (Just (i + 1, q))
        | otherwise     = a : r (Just (1, x))
      f x r Nothing           = r (Just (1, x))

      final (Just a@(i,q)) = [a]
      final Nothing           = []

-- ...which can become a good transformer for list fusion like so
{-# INLINE encode #-}
encode :: (Eq a) => [a] -> [(Int,a)]
encode xs = build (\c n ->
    let
      f x r (Just a@(i,q))  | x == q     = r (Just (i + 1, q))
                         | otherwise = a `c` r (Just (1, x))
      f x r Nothing = r (Just (1, x))

      final (Just a@(i,q)) = a `c` n
      final Nothing           = n

    in
      foldr f final xs Nothing)

-- \end
