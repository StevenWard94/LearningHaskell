{- | A Haskell script to practice using/defining type synonyms
 - Script:          Synonyms.hs
 - Author:          Steven Ward <stevenward94@gmail.com>
 - URL:             https://github.com/StevenWard94/LearningHaskell
 - Last Change:     2016 June 13
 -}

import Data.Map (Map)

type PhoneNumber = String
type Name = String
type PhoneBook = [(Name,PhoneNumber)]

inPhoneBook :: Name -> PhoneNumber -> PhoneBook -> Bool
inPhoneBook name pnum pbook = (name,pnum) `elem` pbook


type AssocList k v = [(k,v)]

type IntegerMap v = Map Integer v
type IntMap = Map Int       -- type parameters can be partially applied like functions
