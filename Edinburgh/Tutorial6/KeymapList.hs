-- |
-- Module:        Edinburgh.Tutorial6.KeymapList
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
-- This document's contents are derived from the file, "KeymapList.hs", in the
-- tutorial's archived contents found online.
--------------------------------------------------------------------------------
--
module
    KeymapList (
                 Keymap,
                 size,
                 get, set, del,
                 select,
                 toList, fromList
                                  ) where

newtype Keymap k a = K [(k,a)]


size :: (Eq k) => Keymap k a -> Int
size (K xs) = length xs

get :: (Eq k) => k -> Keymap k a -> Maybe a
get key (K xs) = lookup key xs

set :: (Eq k) => k -> a -> Keymap k a -> Keymap k a
set key value (K xs) = K (ins xs)
    where
        ins []         = [(key,value)]
        ins ((k,v):xs)
          | k == key    = (k,value) : xs
          | otherwise  = (k,v) : ins xs

del :: (Eq k) => k -> Keymap k a -> Keymap k a
del key (K xs) = K (filter ((/=key).fst) xs)

select :: (Eq k) => (a -> Bool) -> Keymap k a -> Keymap k a
select f (K xs) = K (filter (f.snd) xs)

toList :: (Eq k) => Keymap k a -> [(k,a)]
toList (K xs) = xs

fromList :: (Eq k) => [(k,a)] -> Keymap k a
fromList xs = K xs
