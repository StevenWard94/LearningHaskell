-- |
-- Module:        Edinburgh.Tutorial5
-- Author:        Steven Ward <stevenward94@gmail.com>
-- URL:           https://github.com/StevenWard94/LearningHaskell.d
-- Last Change:   2016 Aug 18
--

-- This module consists of my (attempted) solutions to the fifth Edinburgh Tutorial:
--                                 LOGIC
--           Informatics 1 - Functional Programming: Tutorial 5
--
-- the instructions for which can be found at:
--      http://www.inf.ed.ac.uk/teaching/courses/inf1/fp/#haskell
--
--------------------------------------------------------------------------------
--
module Edinburgh.Tutorial5 where

import Control.Monad ( liftM, liftM2, ap )
import Data.List ( nub, sort, delete, tails )
import Test.QuickCheck ( Arbitrary( arbitrary ), quickCheck, oneof, elements, sized )

-- Warmup \begin

-- INTRODUCTION \begin
-- First, you will write some functions to act on input of the user-defined
-- type, 'Fruit'. The Fruit type constructor has two value constructors:
-- 'Apple String Bool', where the string is the variety and the bool denotes
-- whether the apple has a worm in it or not, and 'Orange String Int', where the
-- string is the same as Apple and the Int value is the number of segments in
-- the orange. For example:
--     Apple "Granny Smith" False    -- a Granny Smith apple w/o a worm
--     Apple "Braeburn" True         -- a Braeburn apple w/ a worm
--     Orange "Sanguinello" 10       -- a Sanguinello orange w/ 10 segments
-- \end

-- EXERCISES \begin

-- First, defining the Fruit data type...
data Fruit = Apple String Bool | Orange String Int

-- Example fruit shown in INTRODUCTION (+1 new):
appleGS, appleBB, orange, orange' :: Fruit
appleGS = Apple "Granny Smith" False
appleBB = Apple "Braeburn" True
orange  = Orange "Sanguinello" 10
orange' = Orange "Seville" 12

fruits :: [Fruit]
fruits = [orange', appleGS, appleBB, orange]

-- Creating a Show instance for Fruit:
instance Show Fruit where
    show (Apple variety worm)   = "Apple( " ++ variety ++ ", " ++ show worm ++ " )"
    show (Orange variety sgmts) = "Orange( " ++ variety ++ ", " ++ show sgmts ++ " )"

-- Exercise 1:
-- Write a function, isBloodOrange, which returns True for blood oranges and
-- False for apples and other oranges. Varieties of blood oranges are Tarocco,
-- Moro and Sanguinello.
isBloodOrange :: Fruit -> Bool
isBloodOrange (Apple _ _) = False
isBloodOrange (Orange var _) = any ((==) var) ["Tarocco", "Moro", "Sanguinello"]


-- Exercise 2:
-- Write a function, bloodOrangeSegments, which returns the total number of
-- blood orange segments in a list of fruit.
segments :: Fruit -> Int
segments f = case f of
               (Apple _ _) -> 0
               (Orange _ s) -> s

bloodOrangeSegments :: [Fruit] -> Int
bloodOrangeSegments = sum . map segments . filter isBloodOrange


-- Exercise 3:
-- Write a function, worms, which returns the number of apples containing worms.
hasWorm :: Fruit -> Bool
hasWorm f = case f of
              (Orange _ _) -> False
              (Apple _ w)  -> w

worms :: [Fruit] -> Int
worms = length . filter hasWorm

--     \end
-- \end

-- Logic \begin

-- INTRODUCTION \begin
-- In the rest of this tutorial, we will implement propositional logic in
-- Haskell. For this, we use the following type and data declarations:
type Name = String
type Names = [Name]
data Prop = Var Name
          | F | T
          | Not Prop
          | Prop :|: Prop
          | Prop :&: Prop

instance Show Prop where
    show  =  showProp

instance Arbitrary Prop where
    arbitrary  =  sized prop
        where
          prop n | n <= 0     =  atom
                 | otherwise =  oneof [ atom
                                      , liftM Not subform
                                      , liftM2 (:|:) subform subform
                                      , liftM2 (:&:) subform subform
                                      ]
                 where
                   atom = oneof [liftM Var (elements ["P","Q","R","S"]), elements [F,T]]
                   subform = prop (n `div` 2)

-- The type 'Prop' is a representation of propositional formulas. Propositional
-- variables such as 'P' and 'Q' can be represented as 'Var "P"' and 'Var "Q"'.
-- Furthermore, we have the Boolean constants 'T' and 'F', the unary connective
-- 'Not' for negation (not to be confused with the function, 'not :: Bool -> Bool'),
-- and the (infix) binary connectives, ':|:' and ':&:' for disjunction (∨) and
-- conjunction (∧), respectively. We will also define the following type synonym:
type Env = [(Name,Bool)]
-- The type, 'Env', is used as an "environment", in which to evaluate
-- a proposition: it is a list of truth assignments for (the names of)
-- propositional variables. Using these types, we will define the following
-- functions:

-- satisfiable:  checks whether a formula is satisfiable (can the whole formula be true)
satisfiable :: Prop -> Bool
satisfiable = or . ap (map . flip eval) (envs . names)

-- eval:  evaluates the given proposition in the given environment (assignment
--      of truth values). For example:
--          *Main> eval [("P", True), ("Q", False)] (Var "P" :|: Var "Q")
--          True
eval :: Env -> Prop -> Bool
eval e (Var x)        = lookUp x e
eval e F              = False
eval e T              = True
eval e (Not p)        = not $ eval e p
eval e (p :|: q)      = eval e p || eval e q
eval e (p :&: q)      = eval e p && eval e q

lookUp :: (Eq a) => a -> [(a,b)] -> b
lookUp z xys = the [ y | (x,y) <- xys, x == z ]
    where the [y] = y
          the _   = error "eval: lookUp: duplicate or missing variable"

-- showProp:  converts a proposition into a readable string, approximating the
--          proper mathematical notation
showProp :: Prop -> String
showProp (Var x)        = x
showProp F              = "F"
showProp T              = "T"
showProp (Not p)        = "(∼" ++ show p ++ ")"
showProp (p :|: q)      = "(" ++ show p ++ " | " ++ show q ++ ")"
showProp (p :&: q)      = "(" ++ show p ++ " & " ++ show q ++ ")"

-- names: returns all variable names contained in a proposition
names :: Prop -> Names
names (Var x)        = [x]
names F              = []
names T              = []
names (Not p)        = names p
names (p :|: q)      = nub $ names p ++ names q
names (p :&: q)      = nub $ names p ++ names q

-- envs:  generates a list of all possible truth assignments for the given list
--      of variables. For example:
--            *Main> envs ["P", "Q"]
--            [[("P",False),("Q",False)],
--             [("P",False),("Q",True)],
--             [("P",True),("Q",False)],
--             [("P",True),("Q",True)]   ]
envs :: Names -> [Env]
envs []      = [[]]
envs (x:xs)  = [ (x,False) : e | e <- envs xs ]
             ++ [ (x,True) : e | e <- envs xs ]

-- functions for printing out truth tables:
centre :: Int -> String -> String
centre w s  =  replicate h ' ' ++ s ++ replicate (w - n - h) ' '
    where n = length s
          h = (w - n) `div` 2

dash :: String -> String
dash = flip replicate ('-') . length

fort :: Bool -> String
fort False  =  "F"
fort True  =  "T"

showTable :: [[String]] -> IO ()
showTable tab  =
    let widths = map length . head $ tab
     in putStrLn $ unlines [ unwords $ zipWith centre widths row | row <- tab ]
