{- | A haskell example-script that replaces 'forever' in CapsLock.hs with 'getContents'
 - Script:        Capslocker.hs
 - Author:        Steven Ward <stevenward94@gmail.com>
 - URL:           https://github.com/StevenWard94/LearningHaskell
 - Last Change:   2016 June 29
 -}

import Data.Char

main = do
    contents <- getContents
    putStr (map toUpper contents)
