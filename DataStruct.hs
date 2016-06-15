{- | A Haskell script to practice creating & working with (recursive) data structures
 - File:          DataStruct.hs
 - Author:        Steven Ward <stevenward94@gmail.com>
 - URL:           https://github.com/StevenWard94/LearningHaskell
 - Last Change:   2016 June 14
 -}

data List a = Empty | Cons a (List a) deriving (Show, Read, Eq, Ord)
data List' b = Empty | Cons { listHead :: b, listTail :: List' b } deriving (Show, Read, Eq, Ord)
-- ^ List' is identical in implementation to List; it is just to show the
-- inner workings of the 'Cons' constructor for List
-- 'Cons' is essentially equivalent to ':'

-- define an infix, 'special-symbol-only', constructor
infixr 5 :-:
data List a = Empty | a :-: (List a) deriving (Show, Read, Eq, Ord)

infixr 5  .++
(.++) :: List a -> List a -> List a
Empty .++ ys = ys
(x :-: xs) .++ ys = x :-: (xs .++ ys)
