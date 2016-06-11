{- | A haskell script to practice applying functions from Data.* modules
 - Module:        Learn.Data
 - Author:        Steven Ward <stevenward94@gmail.com>
 - URL:           https://github.com/StevenWard94/LearningHaskell
 - Last Change:   2016 June 09
 -}

import Data.List
import qualified Data.Map as DM
import qualified Data.Char as DC

numUniques :: (Eq a) => [a] -> Int
numUniques = length . DL.nub


