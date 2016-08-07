-- vim: set tw=80 sw=4 sts=2 ts=8 et fdm=marker fmr=\\begin,\\end fdl=1:
-- |
-- Module:        example_scripts/TypeClass.MakingMonads
-- Author:        Steven Ward <stevenward94@gmail.com>
-- URL:           https://github.com/StevenWard94/LearningHaskell.d
-- Last Change:   2016 Aug 06
--
module TypeClass.MakingMonads where

import Data.Ratio
import Control.Arrow ( first, second )
--
-- I. Making a Prob Monad NewType \begin1
--
newtype Prob a = Prob { getProb :: [(a,Rational)] } deriving Show

instance Functor Prob where
    fmap f (Prob xs) = Prob $ map (first f) xs

-- flattening a probability list of probability lists
-- 25% chance that EXACTLY ONE of 'a' or 'b' will happen
-- both 'a' and 'b' are equally likely to occur
-- 75% change that EXACTLY ONE of 'c' or 'd' will happen
-- both 'c' and 'd' are equally likely to occur
model :: Prob (Prob Char)
model = Prob
    [( Prob [('a',1%2),('b',1%2)] , 1%4 )
    ,( Prob [('c',1%2),('d',1%2)] , 3%4 )]

-- this function is basically the join function from Control.Monad
flatten :: Prob (Prob a) -> Prob a
flatten (Prob xs) = Prob $ concat $ map multAll xs
    where multAll (Prob inner,p) = map (second (p *)) inner

instance Applicative Prob where
    pure x = return x
    Prob fs <*> Prob xs = Prob (zipWith (\(f,p) (x,r) -> (f x,p*r)) fs xs)

instance Monad Prob where
    return x = Prob [(x,1%1)]
    m >>= f = flatten (fmap f m)
    fail _ = Prob []
