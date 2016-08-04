-- |
-- Module:        functor_stuff.hs
-- Author:        Steven Ward <stevenward94@gmail.com>
-- URL:           https://github.com/StevenWard94/LearningHaskell
-- Last Change:   2016 Aug 03
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

------------------------------------------------------------------------------------------
-- The 'newtype' keyword . . .                                                          --
------------------------------------------------------------------------------------------

newtype ZipList a = ZipList { getZipList :: [a] }

newtype CharList = CharList { getCharList :: [Char] } deriving (Eq, Show)


-- example of using 'newtype' to create type class instances
newtype Pair b a = Pair { getPair :: (a,b) }

instance Functor (Pair c) where
--  fmap :: (a -> b) -> Pair c a -> Pair c b
    fmap f (Pair (x,y)) = Pair (f x, y)


-- 'newtype' declarations are not only faster than 'data' declarations,
-- they are also lazier!

--    ghci> undefined
--    *** Exception: Prelude.undefined
--    ghci> head [3,4,5,undefined,2,undefined]
--    3
-- The above was merely to demonstrate how GHC handles the 'undefined'
-- value, for the purpose of understanding context later on...
data CoolBool = CoolBool { getCoolBool :: Bool }

helloMe :: CoolBool -> String
helloMe (CoolBool _) = "hello"
--    ghci> helloMe undefined
--    "*** Exception: Prelude.undefined

newtype KoolBool = KoolBool { getKoolBool :: Bool }
helloMe' :: KoolBool -> String
helloMe' (KoolBool _) = "hello"
--    ghci> helloMe' undefined
--    "hello"

-- the reason for this difference is that the type defined with 'data'
-- could (potentially) have more than one value constructor and/or field
-- and so 'helloMe' must evaluate the CoolBool argument's constructor to
-- ensure that it is, in fact, of the CoolBool type. KoolBool, on the other
-- hand, is defined using 'newtype' and so the compiler knows it can have
-- only one value constructor and one field and thus, does not need to
-- evaluate the value it receives to ensure that it conforms to the
-- (KoolBool _) pattern


------------------------------------------------------------------------------------------
-- 'type' vs. 'newtype' vs. 'data'                                                      --
------------------------------------------------------------------------------------------

-- The 'type' keyword is used only for making "type synonyms", which is
-- when we give another name to a pre-existing type. For example...
type IntList = [Int]    -- this makes IntList & [Int] interchangeable because they are the same thing
--    ghci> ([1,2,3] :: IntList) ++ ([1,2,3] :: [Int])
--    [1,2,3,1,2,3]

-- The 'newtype' keyword is for taking existing types and wrapping them in
-- new types, mostly to make it easier to make them instances of certain
-- type classes. When using 'newtype' to wrap an existing type, the
-- resulting type is separate from the original type. For example...
newtype ChList = ChList { getChList :: [Char] }
-- (++) CANNOT be used to join a ChList and a list of type [Char].
-- (++) CANNOT even join two ChList's because (++) works only on lists and
-- the ChList type isn't a list, even though it could be said that it
-- contains one. However, it is possible to convert two ChList's to [Char]
-- lists, apply (++) to them, and then convert the resulting list to a ChList
-- In practice, 'newtype' declarations can be thought of as 'data'
-- declarations having only ONE CONSTRUCTOR and ONE FIELD. If writing such
-- a 'data' declaration, consider using 'newtype' instead.
