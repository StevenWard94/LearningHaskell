-- |
-- Module:        Edinburgh.Tutorial6.Main
-- Author:        Steven Ward <stevenward94@gmail.com>
-- URL:           https://github.com/StevenWard94/LearningHaskell.d
-- Last Change:   2016 Aug 26
--

-- The containing module consists of my (attempted) solutions to the sixth Edinburgh Tutorial:
--                         THE BARCODE READER
--           Informatics 1 - Functional Programming: Tutorial 6
--
-- the instructions for which can be found at:
--      http://www.inf.ed.ac.uk/teaching/courses/inf1/fp/#haskell
--
--
--
-- This document's contents are derived from the file, "tutorial6.hs", in the
-- tutorial's archived contents found online.
--------------------------------------------------------------------------------
--
module Edinburgh.Tutorial6.Main where

import Control.Monad ( ap )
import Data.List ( intercalate, nub )
import Data.Maybe ( fromJust, isJust )

import System.Random

import KeymapList

-- GENERAL SETUP \begin

-- type synonyms
type Barcode =  String
type Product =  String
type Unit    =  String

type Item    =  (Product,Unit)

type Catalog =  Keymap Barcode Item

-- a simple "test catalog"
testDB :: Catalog
testDB = fromList [
 ("0265090316581", ("The Macannihav'nmor Highland Single Malt", "75ml bottle")),
 ("0903900739533", ("Bagpipes of Glory", "6-CD Box")),
 ("9780201342758", ("Thompson - \"Haskell: The Craft of Functional Programming\"", "Book")),
 ("0042400212509", ("Universal deep-frying pan", "pc"))
 ]

-- Input-Output \begin

readDB :: IO Catalog
readDB = do
            dbl <- readFile "database.csv"
            let db = fromList (map readLine $ lines dbl)
            putStrLn (size db >= 0 `seq` "Done")
            return db

readLine :: String -> (Barcode,Item)
readLine str = (a,(c,b))
    where
        (a,str2) = splitUpon ',' str
        (b,c)    = splitUpon ',' str2

splitUpon :: Char -> String -> (String,String)
splitUpon _ "" = ("","")
splitUpon c (x:xs)
  | x == c     = ("",xs)
  | otherwise = (x:ys,zs)
  where (ys,zs) = splitUpon c xs

getSample :: Catalog -> IO Barcode
getSample db = do
                  g <- newStdGen
                  return $ fst $ toList db !! fst (randomR (0,size db - 1) g)

--    \end
-- \end

-- THE BARCODE READER \begin

-- Introduction...    \begin
---------------------

--  In this tutorial, we will take a look at a (simulated) barcode scanner. By
-- "simulate", we mean searching a database for the item belonging to a given,
-- scanned barcode. We will read the database from a file and then store it in
-- different "shapes" to see which gives the fastest retrieval times. This
-- tutorial also has the associated haskell files: KeymapList.hs and
-- KeymapTree.hs, as well as the database itself: database.csv
--
-- First, we will look at KeymapList.hs:
-- This file defines the abstract data type, "Keymap". The Keymap type is
-- polymorphic and is constructed from two (possibly different) data types, to
-- create its single field, a list of (k,a) pairs. The first data type, k, is
-- the type of the map's keys and the second, a, is the type of the associated
-- values for those keys. The type constructor for the Keymap type, 'K', merely
-- serves as a wrapper to prevent the use of normal list operations on the
-- keymap type. This ensures that we don't accidentally invalidate the
-- representation of a "Keymap" and allows us to change this representation
-- without breaking pre-existing code that uses this type.
--
-- This file ('Main.hs') imports the KeymapList module and then declares
-- a number of type synonyms, one of which - Catalog - being a Keymap that
-- associates values of type 'Item' with keys of type 'Barcode'. Below these
-- declarations if a small test database, called 'testDB'

-- \end

-- Exercise 1    \begin
----------------
-- Before we can work on the database, we need some way of viewing it. If you
-- try 'testDB' (i.e. 'show testDB') in GHCi, you will find that it refuses to
-- print. Using 'toList testDB' will work; however, this looks rather cluttered.

-- 1. (a) Write a function, longestProductLen, that returns the length of the
-- longest product name in a list of (Barcode,Item) pairs.
longestProductLen :: [(Barcode,Item)] -> Int
longestProductLen = maximum . map (length . fst . snd)


-- 1. (b) Write a function, formatLine, which, given a number (i.e. the "column
-- width" for the product name), prints the barcode, product and unit
-- information, separated by dots as a single line. For example:
--     *Main> formatLine 7 ("0001", ("Product", "Unit"))
--     "0001...Product...Unit"
--     *Main> formatLine 7 ("0002", ("Thing", "Unknown"))
--     "0002...Thing.....Unknown"
-- You may assume that the given number is always greater-than or equal-to the
-- length of the product name
formatLine :: Int -> (Barcode,Item) -> String
formatLine n (b,(p,u)) = let dots = repeat '.'
                          in b ++ take 3 dots ++ take n (p ++ dots) ++ u


-- 1. (c) Write a function, showCatalog, that pretty-prints a Catalog. You will
-- need to use KeymapList.toList. Test the function using 'putStr (showCatalog testDB)'.
showCatalog :: Catalog -> String
showCatalog catalog = let catalog'     = toList catalog
                          prodColWidth = longestProductLen catalog' + 3
                       in (intercalate "\n" $ map (formatLine prodColWidth) catalog') ++ "\n"

-- \end

-- Exercise 2    \begin
----------------

-- 2. (b) Write a function, maybeToList, that returns an empty list for
-- 'Nothing' and a singleton list otherwise.
maybeToList :: Maybe a -> [a]
maybeToList = maybe [] return

-- 2. (c) Write another function, listToMaybe, which should behave like a "safe"
-- 'head' function, returning 'Nothing' for an empty list and 'Just head'
-- otherwise.
listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe xs = Just $ head xs


--2. (d) Write the function, catMaybes, which returns a list of the values held
--by 'Just' constructors (all 'Nothing' elements should be stripped).
catMaybes :: [Maybe a] -> [a]
catMaybes = map fromJust . filter isJust

-- \end

-- Exercise 3    \begin
----------------
-- Using the functions from the previous exercise, write a function, getItems,
-- that returns a list of corresponding items from a given list of barcodes.
-- Test your code for errors using the expression:
--     *Main> getItems ["0001", "9780201342758", "0003"] testDB
-- It should return a singleton list of just the Item value for the textbook.
getItems :: [Barcode] -> Catalog -> [Item]
getItems = ((nub . catMaybes) .) . flip (map . flip get)

--    \end
-- \end

