{- | A Haskell script to practice using/defining type synonyms
 - Script:          Synonyms.hs
 - Author:          Steven Ward <stevenward94@gmail.com>
 - URL:             https://github.com/StevenWard94/LearningHaskell
 - Last Change:     2016 June 13
 -}

import qualified Data.Map as DM

type PhoneNumber = String
type Name = String
type PhoneBook = [(Name,PhoneNumber)]

inPhoneBook :: Name -> PhoneNumber -> PhoneBook -> Bool
inPhoneBook name pnum pbook = (name,pnum) `elem` pbook

type AssocList k v = [(k,v)]

type IntegerMap v = DM.Map Integer v
type IntMap = DM.Map Int       -- type parameters can be partially applied like functions



data LockerState = Taken | Free deriving (Show, Eq)

type Code = String

type LockerMap = Map.Map Integer (LockerState, Code)

lockerLookup :: Integer -> LockerMap -> Either String Code
lockerLookup lockerNum map =
  case Map.lookup lockerNum map of
    Nothing -> Left $ "Locker number " ++ show lockerNum ++ " doesn't exist!"
    Just (state, code) -> if state /= Taken
                        then Right code
                        else Left $ "Locker number " ++ show lockerNum ++ " is already taken!"

exLockerMap :: LockerMap
exLockerMap = Map.fromList
      [(100,(Taken,"ZD39I"))
      ,(101,(Free,"JAH3I"))
      ,(103,(Free,"IQSA9"))
      ,(105,(Free,"QOTSA"))
      ,(109,(Taken,"893JJ"))
      ,(110,(Taken,"99292"))
      ]
