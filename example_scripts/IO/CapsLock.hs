{- | A haskell script intended to be a simple example of how to use the 'forever' IO function
 - Script:        CapsLock.hs
 - Author:        Steven Ward <stevenward94@gmail.com>
 - URL:           https://github.com/StevenWard94/LearningHaskell
 - Last Change:   2016 June 28
 -}

import Control.Monad
import Data.Char

main = forever $ do
    putStr "Tell me something: "
    something <- getLine
    putStrLn $ map toUpper something
