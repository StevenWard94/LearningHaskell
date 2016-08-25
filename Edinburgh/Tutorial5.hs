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
          | Prop :->: Prop
          | Prop :<->: Prop
          deriving (Eq, Ord)

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
                                      , liftM2 (:->:) subform subform
                                      , liftM2 (:<->:) subform' subform'
                                      ]
                 where
                   atom = oneof [liftM Var (elements ["P","Q","R","S"]), elements [F,T]]
                   subform  = prop (n `div` 2)
                   subform' = prop (n `div` 4)

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
eval e (p :->: q)     = not (eval e p) || eval e q
eval e (p :<->: q)    = eval e p == eval e q

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
showProp (Not p)        = "(~" ++ show p ++ ")"
showProp (p :|: q)      = "(" ++ show p ++ " | " ++ show q ++ ")"
showProp (p :&: q)      = "(" ++ show p ++ " & " ++ show q ++ ")"
showProp (p :->: q)     = "(" ++ show p ++ " -> " ++ show q ++ ")"
showProp (p :<->: q)    = "(" ++ show p ++ " <-> " ++ show q ++ ")"

-- names: returns all variable names contained in a proposition
names :: Prop -> Names
names (Var x)        = [x]
names F              = []
names T              = []
names (Not p)        = names p
names (p :|: q)      = nub $ names p ++ names q
names (p :&: q)      = nub $ names p ++ names q
names (p :->: q)     = nub $ names p ++ names q
names (p :<->: q)    = nub $ names p ++ names q

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

tables :: [Prop] -> IO ()
tables ps  =
  let xs = nub $ concatMap names ps
   in showTable $
       [ xs            ++ [ "|" ] ++ [showProp p | p <- ps]           ] ++
           [ dashvars xs   ++ [ "|" ] ++ [dash (showProp p) | p <- ps] ] ++
               [ evalvars e xs ++ [ "|" ] ++ [fort (eval e p) | p <- ps] | e <- envs xs]
    where dashvars xs       =  [ dash x | x <- xs ]
          evalvars e xs     =  [ fort $ eval e (Var x) | x <- xs ]

table :: Prop -> IO ()
table p = tables [p]
-- \end

-- EXERCISES \begin

-- Exercise 4 \begin
-- Write the following formulas as 'Props' (call them 'p1', 'p2', and 'p3').
-- Then use 'satisfiable' to check their satisfiability and 'table' to print
-- their truth tables.

-- (a) ((P ∨ Q) & (P & Q))
p1 :: Prop
p1  =  (Var "P" :|: Var "Q") :&: (Var "P" :&: Var "Q")

p1Satisfiable :: Bool
p1Satisfiable  =  satisfiable p1

p1Table :: IO ()
p1Table  =  table p1


-- (b) ((P ∨ Q) & ((¬P) & (¬Q)))
p2 :: Prop
p2  =  (Var "P" :|: Var "Q") :&: (Not (Var "P") :&: Not (Var "Q"))

p2Satisfiable :: Bool
p2Satisfiable  =  satisfiable p2

p2Table :: IO ()
p2Table  =  table p2


-- (c) ((P & (Q ∨ R)) & (((¬P) ∨ (¬Q)) & ((¬P) ∨ (¬R))))
p3 :: Prop
p3  =  (Var "P" :&: (Var "Q" :|: Var "R")) :&:
       ((Not (Var "P") :|: Not (Var "Q")) :&: (Not (Var "P") :|: Not (Var "R")))

p3Satisfiable :: Bool
p3Satisfiable  =  satisfiable p3

p3Table :: IO ()
p3Table  =  table p3

-- \end

-- Exercise 5 \begin

-- 5. (a) A proposition is a "tautology" if it is always true (i.e. in every
-- possible environment). Using 'names', 'envs' and 'eval', write a function,
-- tautology, which checks whether the given proposition is a tautology. Test it
-- on the examples from Exercise (4) and on their negations.
tautology :: Prop -> Bool
tautology = and . ap (map . flip eval) (envs . names)

-- 5. (b) Create two QuickCheck tests to verify that tautology is working
-- correctly. Use the following facts as the basis for your test properties:
--     For any property P,
--       i. either P is a tautology, or ¬P is satisfiable
--      ii. either P is not satisfiable, or ¬P is not a tautology

prop_tautI :: Prop -> Bool
prop_tautI  =  liftM2 (||) satisfiable (tautology . Not)

prop_tautII :: Prop -> Bool
prop_tautII  =  ap ((||) . not . satisfiable) (not . tautology . Not)

prop_taut :: Prop -> Bool
prop_taut  =  liftM2 (==) tautology (not . satisfiable . Not)

-- \end

-- Exercise 6 \begin
-- We will extend the datatype and functions for propositions to handle the
-- connectives, → (implication) and ↔ (bi-implication, or "iff"). They will be
-- implemented as the constructors, ':->:' and ':<->:'. After you have
-- implemented them, the truth tables for both should be as follows:
--
--   *Main> table (Var "P" :->: Var "Q")        *Main> table (Var "P" :<->: Var "Q")
--   P Q | (P->Q)                               P Q | (P<->Q)
--   - - | ------                               - - | -------
--   F F |   T                                  F F |    T
--   F T |   T                                  F T |    F
--   T F |   F                                  T F |    F
--   T T |   T                                  T T |    T

-- 6. (a) Go to the declaration of the 'Prop' datatype above and extend it with
-- the infix constructors, ':->:' and ':<->:'
-- SEE DATA DECLARATION IN INTRODUCTION

-- 6. (b) Extend the definitions for the 'showProp', 'eval' and 'names'
-- functions to cover the new constructors :->: and :<->:. Test your definitions
-- by printing out the truth tables above.

-- 6. (c) Define the following formulas as 'Prop's, check their
-- satisfiability and print their truth tables.

-- (c)   i.  ((P → Q) & (P ↔ Q))
p4 :: Prop
p4  =  (Var "P" :->: Var "Q") :&: (Var "P" :<->: Var "Q")

-- (c)  ii.  ((P → Q) & (P & ¬Q))
p5 :: Prop
p5  =  (Var "P" :->: Var "Q") :&: (Var "P" :&: Not (Var "Q"))

-- (c) iii.  ((P ↔ Q) & ((P & ¬Q) ∨ (¬P & Q)))
p6 :: Prop
p6  =  (Var "P" :<->: Var "Q") :&: ((Var "P" :&: Not (Var "Q")) :|: (Not (Var "P") :&: Var "Q"))

truthTables_p4p5p6 :: IO ()
truthTables_p4p5p6 = tables [p4,p5,p6]
-- \end

-- Exercise 7 \begin
-- Two formulas are "equivalent" if they always have the same truth values,
-- regardless of the values of their propositional variables. In other words,
-- formulas are equivalent if, in any given environment, they are either both
-- true or both false

-- 7. (a) Write a function, equivalent, that returns True only when two
-- propositions are equivalent in the above sense.
equivalent :: Prop -> Prop -> Bool
equivalent p q = let envList = envs $ nub (names p ++ names q)
                  in and [ eval e p == eval e q | e <- envList ]

-- 7. (b) Write another version of 'equivalent', this time by combining the two
-- arguments into a larger proposition and then using 'tautology' or
-- 'satisfiable' to evaluate it.
equivalent' :: Prop -> Prop -> Bool
equivalent' = (tautology .) . (:<->:)

-- 7. (c) Write a QuickCheck test property to verify that the two versions of
-- 'equivalent' are equivalent.
prop_equivalent :: Prop -> Prop -> Bool
prop_equivalent p q = equivalent p q == equivalent' p q

-- \end

-- The "subformulas" of a proposition are defined as follows:
--
--   ∙ A propositional letter 'P' or a constant 't' or 'f' has itself as its
--   only subformula.
--
--   ∙ A proposition of the form '¬P' has as subformulas, itself and all
--   subformulas of 'P'
--
--   ∙ A proposition of the form 'P & Q', 'P ∨ Q', 'P → Q', or 'P ↔ Q' has as
--   subformulas, itself and all subformulas of 'P' and 'Q'
--
-- The function 'fullTable' defined below prints out a truth table for
-- a formula, with an additional column for each of the formula's non-trivial
-- subformulas.
fullTable :: Prop -> IO ()
fullTable = tables . filter nontrivial . subformulas
    where nontrivial :: Prop -> Bool
          nontrivial (Var _) = False
          nontrivial T       = False
          nontrivial F       = False
          nontrivial _       = True

-- Exercise 8 \begin
-- Define a function, subformulas, that returns all of the subformulas of
-- a given formula. For example:
--     *Main> map showProp (subformulas p2)
--     ["((P|Q)&((~P)&(~Q)))","(P|Q)","P","Q","((~P)&(~Q))","(~P)","(~Q)"]
-- (We need to use 'map showProp' here in order to convert each proposition into
-- a distinct string; otherwise, we could not easily view the results.)
-- After defining the function, test out 'subformulas' and 'fullTable' on each
-- of the Props you defined earlier (i.e. p1 - p6).
subformulas :: Prop -> [Prop]
subformulas (Not p)      =  Not p : subformulas p
subformulas (p :|: q)    =  (p :|: q)   : nub (subformulas p ++ subformulas q)
subformulas (p :&: q)    =  (p :&: q)   : nub (subformulas p ++ subformulas q)
subformulas (p :->: q)   =  (p :->: q)  : nub (subformulas p ++ subformulas q)
subformulas (p :<->: q)  =  (p :<->: q) : nub (subformulas p ++ subformulas q)
subformulas p            =  [p]

--     \end
--   \end
-- \end

-- Optional Material \begin

-- NORMAL FORMS
-- In this part of the tutorial we will put propositional formulas into several
-- different "normal forms". First, we will deal with "negation" normal form. As
-- a reminder, a formula is in "negation normal form" if it consists of ONLY the
-- connectives, '∨' and '&', unnegated propositional variables, 'P', and negated
-- propositional variables, '¬P', and the constants 't' and 'f'. Thus, negation
-- is ONLY APPLIED TO PROPOSITIONAL VARIABLES AND NOTHING ELSE.
--
-- To transform a formula into "negation normal form", you may want to use the
-- following equivalences:
--                 ¬(P & Q) ⇔ (¬P) ∨ (¬Q)
--                 ¬(P ∨ Q) ⇔ (¬P) & (¬Q)
--                  (P → Q) ⇔ (¬P) ∨ Q
--                  (P ↔ Q) ⇔ (P → Q) & (Q → P)
--                    ¬(¬P) ⇔ P

-- Exercise 9:
-- Write a function, isNNF, to test whether a Prop is in negation normal form
isNNF :: Prop -> Bool
isNNF (p :|: q)      =  isNNF p && isNNF q
isNNF (p :&: q)      =  isNNF p && isNNF q
isNNF (Not (Var _))  =  True
isNNF (Var _)        =  True
isNNF T              =  True
isNNF F              =  True
isNNF _              =  False


-- Exercise 10:
-- Write a function, toNNF, that puts an arbitrary Prop into negation normal
-- form. Use the test properties 'prop_NNF1' and 'prop_NNF2' to verify that your
-- function is correct. (Hint: don't be alarmed if you need many case
-- distinctions).
toNNF :: Prop -> Prop
toNNF = nnorm
    where nnorm (Not (p :|: q))    =  nnorm (Not p) :&: nnorm (Not q)
          nnorm (Not (p :&: q))    =  nnorm (Not p) :|: nnorm (Not q)
          nnorm (Not (Not x))      =  nnorm x
          nnorm (Not F)            =  T
          nnorm (Not T)            =  F
          nnorm (Not (p :->: q))   =  nnorm (Not (nnorm (p :->: q)))
          nnorm (Not (p :<->: q))  =  nnorm (Not (nnorm (p :<->: q)))
          nnorm (p :->: q)         =  nnorm (Not p) :|: nnorm q
          nnorm (p :<->: q)        =  nnorm (p :->: q) :&: nnorm (q :->: p)
          nnorm (p :|: q)          =  nnorm p :|: nnorm q
          nnorm (p :&: q)          =  nnorm p :&: nnorm q
          nnorm x                  =  x

prop_NNF1 :: Prop -> Bool
prop_NNF1 = isNNF . toNNF

prop_NNF2 :: Prop -> Bool
prop_NNF2 = ap equivalent toNNF


-- Next, we will turn a formula into "conjunction" normal form. This means that
-- the formula is a "conjunction of clauses", and a clause is a disjunction of
-- (possibly negated) propositional variables, called "atoms". You will need to
-- pay special attention to the constants 't' and 'f'. The Props, 'T' and 'F',
-- themselves are considered to be in "conjunction" normal form, but otherwise
-- should not occur within formulas in normal form. They can be eliminated using
-- the following equivalences:
--                 (P & t) ⇔ (t & P) ⇔ P
--                 (P & f) ⇔ (f & P) ⇔ f
--                 (P ∨ t) ⇔ (t ∨ P) ⇔ t
--                 (P ∨ f) ⇔ (f ∨ P) ⇔ P

-- Exericise 11:
-- Write a function, isCNF, to test if a Prop is in conjunction normal form.
isCNF :: Prop -> Bool
isCNF T          =  True
isCNF F          =  True
isCNF (p :&: q)
    | or [p == T, p == F, q == T, q == F] = False
    | otherwise                       = and [isCNF p, isCNF q]
isCNF p          = clause p
    where clause (p :|: q)  =  clause p && clause q
          clause p          =  literal p
              where literal (Not (Var _))  =  True
                    literal (Var _)        =  True
                    literal _              =  False


-- A common way of expressing formulas in conjunctive normal form is as a list
-- of lists, where the inner lists represent the clauses. Thus:
--                 ((A ∨ B) & ((C ∨ D) ∨ E)) & G  ⇔  [[A,B],[C,D,E],[G]]


-- Exercise 13:
-- Write a function, listsToCNF, to translate a list of lists of Props (which
-- you may assume to be variables or negated varaibles) to a Prop in conjunctive
-- normal form.
listsToCNF :: [[Prop]] -> Prop
listsToCNF pss
    |  null pss      =  T
    |  any null pss  =  F
    |  otherwise     =  foldl1 (:&:) . map (foldl1 (:|:)) $ pss


-- Exercise 14:
-- Write a function, listsFromCNF, to write a formula in conjunctive normal form
-- as a list of lists
listsFromCNF :: Prop -> [[Prop]]
listsFromCNF p
    |  not $ isCNF p  =  error "listsFromCNF: proposition not in conjunctive normal form"
    |  otherwise    =  decompose p
    where decompose (p :&: q)  =  decompose p ++ decompose q
          decompose p          =  [literals p]
          literals  (p :|: q)  =  literals p ++ literals q
          literals  p          =  [p]
