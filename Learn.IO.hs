{- | A haskell script to help learn IO operations
 - File:          Learn.IO.hs
 - Author:        Steven Ward <stevenward94@gmail.com>
 - URL:           https://github.com/StevenWard94/LearningHaskell
 - Last Change:   2016 June 25
 -}

main = do
    putStrLn "Hello, what is your name?"
    name <- getLine
    putStrLn ("Hey " ++ name ++ ", you rock!")
