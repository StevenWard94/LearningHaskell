-- |
-- Module:        functor_stuff.hs
-- Author:        Steven Ward <stevenward94@gmail.com>
-- URL:           https://github.com/StevenWard94/LearningHaskell
-- Last Change:   2016 July 28
--

-- dummy module to remove 'main not defined in Main' warning
module FunctorStuff where

import Data.Char
import Data.List
import Control.Applicative

main' = do line <- fmap (intersperse '-' . reverse . map toUpper) getLine
           putStrLn line

-- example of a type constructor that can be made an instance of the
-- Functor typeclass without GHC reporting an error BUT IS NOT truly
-- a functor because it fails to satisfy the two 'functor laws'
data CMaybe a = CNothing | CJust Int a deriving (Show)

instance (Eq a) => Eq (CMaybe a) where
    cmA == cmB = case (cmA,cmB) of
                   (CNothing,CNothing) -> True
                   ((CJust x a),(CJust y b)) -> (x,a) == (y,b)
                   _ -> False

instance Functor CMaybe where
    fmap f CNothing = CNothing
    fmap f (CJust counter x) = CJust (counter + 1) (f x)

funcLawOne :: CMaybe a -> CMaybe a
funcLawOne = fmap (\x -> x)

testLawOne :: (Eq a) => CMaybe a -> Bool
testLawOne = \m -> m == funcLawOne m

funcLawTwo :: (Eq a) => (a -> a) -> (a -> a) -> CMaybe a -> (CMaybe a, CMaybe a)
funcLawTwo f g cm =
    let resComp = fmap (f . g) cm
        resExpl = fmap f (fmap g cm)
     in (resComp,resExpl)

testLawTwo :: (Eq a) => (a -> a) -> (a -> a) -> CMaybe a -> Bool
testLawTwo f g cm = fst lawTest == snd lawTest where lawTest = funcLawTwo f g cm

-- some examples of Applicative instance implementations
data PMaybe a = PNothing | PJust a deriving (Show)

instance Functor PMaybe where
    fmap _ PNothing = PNothing
    fmap f (PJust x) = PJust (f x)

instance Applicative PMaybe where
    pure = PJust
    PNothing <*> _ = PNothing
    (PJust f) <*> something = fmap f something

data List a = Empty | List [a] deriving (Show)

instance Functor List where
    fmap _ Empty = Empty
    fmap _ (List []) = List []
    fmap f (List [x]) = List [f x]
    fmap f (List xs) = List (map f xs)

instance Applicative List where
    pure x = List [x]
    (List fs) <*> (List xs) = List [f x | f <- fs, x <- xs]

data MyZipList a = MyZipList [a] deriving (Show)

instance Functor MyZipList where
    fmap _ (MyZipList []) = MyZipList []
    fmap f (MyZipList [x]) = MyZipList[f x]
    fmap f (MyZipList xs) = MyZipList [ f x | x <- xs ]

instance Applicative MyZipList where
    pure x = MyZipList (repeat x)
    MyZipList fs <*> MyZipList xs = MyZipList (zipWith (\f x -> f x) fs xs)


-- Example of how 'sequencing' affects the implementation of IO as an
-- instance of Applicative
myAction :: IO String
myAction = do
    a <- getLine
    b <- getLine
    return $ a ++ b

myApplicativeAction :: IO String
myApplicativeAction = (++) <$> getLine <*> getLine


-- some more messing around with Control.Applicative
liftA2' :: (Applicative f) => (a -> b -> c) -> f a -> f b -> f c
liftA2' f a b = f <$> a <*> b

sequenceA' :: (Applicative f) => [f a] -> f [a]
sequenceA' [] = pure []
sequenceA' (x:xs) = (:) <$> x <*> sequenceA xs

sequenceB :: (Applicative f) => [f a] -> f [a]
sequenceB = foldr (liftA2 (:)) (pure [])
