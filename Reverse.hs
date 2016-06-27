{- | A haskell script that will repeatedly reverse input lines until an empty line is input
 - Script:        Reverse.hs
 - Author:        Steven Ward <stevenward94@gmail.com>
 - URL:           https://github.com/StevenWard94/LearningHaskell
 - Last Change:   2016 June 26
 -}


main = do
    line <- getLine
    if null line
       then return ()
       else do
           putStrLn $ reverseWords line
           main

reverseWords :: String -> String
reverseWords = unwords . map reverse . words
