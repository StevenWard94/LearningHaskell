-- vim: set foldmethod=marker foldmarker=\\begin,\\end:

-- |
-- Module:        Monads.Writer
-- Author:        Steven Ward <stevenward94@gmail.com>
-- URL:           https://github.com/StevenWard94/LearningHaskell.d
-- Last Change:   2016 Aug 04
--

module Monads.Writer where

import Data.Monoid

--
-- I. Introduction \begin1
--

-- A function that takes an Integer and returns a Boolean value and a "log"
-- String in a tuple. This tuple is sort of like a Monad b/c it takes
-- a value and puts it with a context (i.e. a "log")
isBigGang :: Integer -> (Bool,String)
isBigGang x = (x > 9, "Gang size compared with 9.")

-- Here is a function that applies a "log-returning" function to
-- a (value,log) tuple, where 'log' is a String (the context...) and
-- 'value' is any type, 'a' (sort of like 'Maybe a' or [a])
applyLog :: (a,String) -> (a -> (b,String)) -> (b,String)
applyLog (x,log) f = let (y,newLog) = f x
                      in (y,log ++ newLog)

-- Since applyLog uses (++) to append logs (Strings), it would technically
-- work with a list of any type (instead of only lists of type [Char]). For
-- that matter, it would work with ANY MONOID if (++) was replaced by mappend
applyM :: (Monoid m) => (a,m) -> (a -> (b,m)) -> (b,m)
applyM (a,xm) f = let (b,ym) = f a
                   in (b,xm `mappend` ym)

-- another little example of this value w/ monoid value behavior...
type Food = String
type Price = Sum Int

addDrink :: Food -> (Food,Price)
addDrink "beans" = ("milk", Sum 25)
addDrink "jerky" = ("whiskey", Sum 99)
addDrink _ = ("beer", Sum 30)

--
-- II. The Writer Type \begin1
--
-- this is a reimplementation of the Writer monad in Control.Monad.Writer
newtype Writer w a = Writer { runWriter :: (a,w) } deriving (Show, Read, Eq)

instance Functor (Writer w) where
    fmap f (Writer (x,m)) = Writer (f x, m)

instance (Monoid w) => Applicative (Writer w) where
    pure x = Writer (x, mempty)
    Writer (f,m) <*> rightWrite = fmap f rightWrite

-- now to make it an instance of Monad; note that >>= is basically applyM
instance (Monoid w) => Monad (Writer w) where
    return x = Writer (x, mempty)
    (Writer (x,v)) >>= f = let (Writer (y,v')) = f x
                           in Writer (y, v `mappend` v')


--
-- III. Using the Writer Type \begin1
--

-- normally these would require 'import Control.Monad.Writer'

-- the 'tell' and 'writer' functions are part of the definition for MonadWriter type class
writer :: (Monoid w, Monad m) => (a,w) -> m a
writer ~(a,w) = do
    tell w
    return a

tell :: (Monoid w, Monad m) => w -> m ()
tell w = writer ((),w)

logNumber :: Integer -> Writer [String] Integer
logNumber x = Writer (x, ["Got number: " ++ show x])

multWithLog :: Writer [String] Integer
multWithLog = do
    a <- logNumber 3
    b <- logNumber 5
    return (a * b)

multLogVerbose :: Writer [String] Integer
multLogVerbose = do
    a <- logNumber 3
    b <- logNumber 5
    tell ["Gonna multiply these two"]
    return (a*b)

--
-- IV. Adding "Logging" to Programs \begin1
--

-- reimplementation of gcd function w/ additional logging capabilities
gcd' :: Integer -> Integer -> Integer
gcd' a b
  | b == 0     = a
  | otherwise = gcd' b (a `mod` b)

gcdLog :: Integer -> Integer -> Writer [String] Integer
gcdLog a b
  | b == 0 = do
      tell ["Finished with " ++ show a]
      return a
  | otherwise = do
      tell [show a ++ " % " ++ show b ++ " = " ++ show (a `mod` b)]
      gcdLog b (a `mod` b)

-- Difference Lists
newtype DiffList a = DiffList { getDiffList :: [a] -> [a] }

toDiffList :: [a] -> DiffList a
toDiffList xs = DiffList (xs++)

fromDiffList :: DiffList a -> [a]
fromDiffList (DiffList f) = f []
