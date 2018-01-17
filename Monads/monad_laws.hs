-- |
-- Module:        monad_laws.hs
-- Author:        Steven Ward <stevenward94@gmail.com>
-- URL:           https://github.com/StevenWard94/LearningHaskell.d
-- Last Change:   2016 Aug 04
--

module Laws.Monad where

------------------------------------------------------------------------------------------
--        MONAD LAWS
------------------------------------------------------------------------------------------
-- Much like the Functor and Applicative type classes, instances of the
-- Monad type class may not REALLY be monads if they fail to satisfy the
-- monad laws

--
-- 1. Left Identity
--      return x >>= f ≡ f x
leftIdTest :: (Monad m, Eq (m b)) => a -> (a -> m b) -> Bool
leftIdTest x f = feedResult == applyResult
    where feedResult  = return x >>= f
          applyResult = f x

--
-- 2. Right Identity
--      For a monadic value, m:
--        m >>= return ≡ m
rghtIdTest :: (Monad m, Eq (m a)) => m a -> Bool
rghtIdTest m = m == (m >>= return)

--
-- 3. Associativity
--      For a monadic value. m:
--        (m >>= f) >>= g ≡ m >>= (\x -> f x >>= g)
-- first, an operator for composing two monadic functions
(<=<) :: (Monad m) => (b -> m c) -> (a -> m b) -> (a -> m c)
f <=< g = \x -> g x >>= f

assocTest :: (Monad m, Eq (m c)) => m a -> (a -> m b) -> (b -> m c) -> Bool
assocTest m f g = chainResult == assocResult
    where chainResult = m >>= f >>= g
          assocResult = m >>= (g <=< f)
