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
