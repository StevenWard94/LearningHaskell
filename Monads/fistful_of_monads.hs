-- |
-- Module:        fistful_of_monads.hs
-- Author:        Steven Ward <stevenward94@gmail.com>
-- URL:           https://github.com/StevenWard94/LearningHaskell.d
-- Last Change:   2016 Aug 04
--

module Fistful where

import Prelude hiding ( (>>=) )
import qualified Prelude as P
import qualified Control.Monad as CM
-- |
-- Monads are a natural extension of applicative functors and with them,
-- we're concerned with this: if you have a value w/ a context (e.g. m a),
-- how do you apply to it a function that takes a normal 'a' and returns
-- a value with a context? That is, how do you apply a function of type
-- (a -> mb) to a value of type m a? Essentially, we will want this function:
--            (>>=) :: (Monad m) => m a -> (a -> m b) -> m b

-- Although we write m a (as opposed to f a) because we are dealing with
-- Monads, they are really just applicative functors that also support the
-- "bind" function (>>=)

data PMaybe a = PNothing | PJust a
    deriving (Show, Read, Eq)

instance Functor PMaybe where
    fmap f PNothing = PNothing
    fmap f (PJust x) = PJust (f x)

instance Applicative PMaybe where
    pure x = PJust x
    PNothing <*> _ = PNothing
    PJust f <*> psomething = fmap f psomething

-- (>>=) :: PMaybe a -> (a -> PMaybe b) -> PMaybe b
applyMaybe :: PMaybe a -> (a -> PMaybe b) -> PMaybe b
applyMaybe PNothing f = PNothing
applyMaybe (PJust x) f = f x

-- For demonstration's sake, here's a reimplementation of the Monad type class
class Danom d where
    return :: a -> d a

    (>>=) :: d a -> (a -> d b) -> d b

    (>>) :: d a -> d b -> d b
    x >> y = x >>= \_ -> y

    fail :: String -> d a
    fail msg = error msg


instance Monad PMaybe where
    return x = PJust x
    PNothing >>= f = PNothing
    PJust x >>= f  = f x
    fail _ = PNothing


-- problem to illustrate monadic processing
type Birds = Int
type Pole  = (Birds, Birds)

landLeft :: Birds -> Pole -> Maybe Pole
landLeft n (left,right)
  | abs (left + n - right) < 4 = Just (left + n, right)
  | otherwise                  = Nothing

landRight :: Birds -> Pole -> Maybe Pole
landRight n (left,right)
  | abs (left - right + n) < 4 = Just (left, right + n)
  | otherwise                  = Nothing

routine :: Maybe Pole
routine = case landLeft 1 (0,0) of
    Nothing -> Nothing
    Just pole1 -> case landRight 4 pole1 of
        Nothing -> Nothing
        Just pole2 -> case landLeft 2 pole2 of
            Nothing -> Nothing
            Just pole3 -> landLeft 1 pole3

infixl 7 -:
(-:) :: a -> (a -> b) -> b
x -: f = f x


instance Danom Maybe where
    return x = Just x
    Nothing >>= f = Nothing
    Just x >>= f = f x
    fail _ = Nothing


foo :: Maybe String
foo = Just 3   >>= (\x ->
      Just "!" >>= (\y ->
      Just (show x ++ y)))

dofoo :: Maybe String
dofoo = do
    x <- Just 3
    y <- Just "!"
    Just (show x ++ y)

marySue :: Maybe Bool
marySue = do
    x <- Just 9
    Just (x > 8)

doRoutine :: Maybe Pole
doRoutine = do
    start <- P.return (0,0)
    first <- landLeft 2 start
    second <- landRight 2 first
    landLeft 1 second

banana :: Maybe Pole
banana = do
    start <- P.return (0,0)
    first <- landLeft 2 start
    Nothing
    second <- landRight 2 first
    landLeft 1 second


justH :: Maybe Char
justH = do
    (x:xs) <- Just "hello"
    P.return x

wopwop :: Maybe Char
wopwop = do
    (x:xs) <- Just ""
    P.return x


instance Danom [] where
    return x = [x]
    xs >>= f = concatMap f xs
    fail _ = []

listOfTuples :: [(Int,Char)]
listOfTuples = do
    n <- [1,2]
    ch <- ['a','b']
    P.return (n,ch)

------------------------------------------------------------------------
-- MonadPlus type class is for monads that can act like monoids
------------------------------------------------------------------------
class Danom m => DanomPlus m where
    mzero :: m a
    mplus :: m a -> m a -> m a

instance DanomPlus [] where
    mzero = []
    mplus = (++)

guard :: (CM.MonadPlus m) => Bool -> m ()
guard True = P.return ()
guard False = CM.mzero

sevensOnly :: [Int]
sevensOnly = do
    x <- [1..50]
    guard ('7' `elem` show x)
    P.return x
