{- | A simple script that takes 'girlfriend.txt', changes it to all CAPS & writes the tweaked content to 'girlfriendcaps.txt'
 - Script:        girlfriend2caps.hs
 - Author:        Steven Ward <stevenward94@gmail.com>
 - URL:           https://github.com/StevenWard94/LearningHaskell
 - Last Change:   2016 July 4
 -}

import System.IO
import Data.Char

main = do
    contents <- readFile "girlfriend.txt"
    writeFile "girlfriendcaps.txt" (map toUpper contents)

--    writeFile :: FilePath -> String -> IO ()
