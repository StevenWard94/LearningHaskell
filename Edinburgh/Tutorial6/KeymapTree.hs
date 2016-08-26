-- |
-- Module:        Edinburgh.Tutorial6.KeymapTree
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
-- This document's contents are derived from the file, "KeymapTree.hs", in the
-- tutorial's archived contents found online.
--------------------------------------------------------------------------------
--

module
    KeymapTree (
                 Keymap,
                 size, depth,
                 get, set, del,
                 select,
                 toList, fromList,
                 merge, filterLT, filterGT
                                           ) where

-- Modules for testing

import Test.QuickCheck
import Control.Monad
import Data.List

-- The data type

data Keymap k a  = Leaf
                 | Node k a (Keymap k a) (Keymap k a)

-- A test tree

testTree :: Keymap Int Int
testTree = Node 2 20 ( Node 1 10 Leaf Leaf )
                     ( Node 3 30 Leaf 
                               ( Node 4 40 Leaf Leaf ) )

-- Instances for QuickCheck
instance (Ord k, Show k, Show a) => Show (Keymap k a) where
    show = show . toList

instance (Ord k, Arbitrary k, Arbitrary a) => Arbitrary (Keymap k a) where
    arbitrary = liftM fromList $ liftM2 zip (liftM nub arbitrary) arbitrary
