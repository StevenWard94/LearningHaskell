-- |
-- Module:        Monads.Trivial
-- Author:        Steven Ward <stevenward94@gmail.com>
-- URL:           https://github.com/StevenWard94/LearningHaskell.d
-- Last Change:   2016 Aug 09
--

-- This is based off of a blog article entitled, "The Trivial Monad", which can
-- be found at:
--     http://blog.sigfpe.com/2007/04/trivial-monad.html

module Monads.Trivial where

import Prelude hiding ( fmap, return )
import qualified Prelude as P

import Control.Monad ( (>=>) )

-- | A "wrapper" type that does nothing other than hold any other type.
--   This type allows wrapping but DOES NOT allow unwrapping.
data W a = W a deriving Show

-- I. Initial Function (Theory) \begin
-- | A way to wrap any type we want in the wrapper class
return :: a -> W a
return x = W x

-- | A way to manipulate wrapped data without unwrapping it
fmap :: (a -> b) -> W a -> W b
fmap f (W x) = W $ f x

-- | A way to unwrap, manipulate and rewrap functions without the end-user ever
-- being able to unwrap the data themselves.
bind :: (a -> W b) -> W a -> W b
bind f (W x) = f x
--                             \end

-- II. Exercises 1 and 2 \begin

-- | Exercise 1:
-- Define a function, g :: Int -> W Int -> W Int, s.t., g x (W y) ≡ W (x+y)
g :: Int -> W Int -> W Int
g x w = bind (return . (+x)) w

-- | Exercise 2:
-- Define a function, h :: W Int -> W Int -> W Int, s.t., h (W x) (W y) ≡ W (x+y)
h :: W Int -> W Int -> W Int
h w v = bind (flip g v) w
--                             \end

-- III. Defining W Instance of Monad \begin

-- Now, to make the "wrapper" type, W, an instance of the Monad type class...

-- first, it needs to be a Functor...
instance Functor W where
    fmap = fmap

-- next, it needs to be an Applicative Functor...
instance Applicative W where
    pure = return
    W f <*> w = fmap f w

-- and finally, to make it a Monad...
instance Monad W where
    return = return               -- thus: return x = W x
    w >>= f = bind f w   -- thus: W x >>= f = f x
--                                    \end

-- Exercises 3 and 4 \begin

-- | Exercise 4:
-- Define a function, join :: W (W a) -> W a, using the Monad API and NO EXPLICIT UNWRAPPING
join :: W (W a) -> W a
join w = w >>= id
--                   \end
