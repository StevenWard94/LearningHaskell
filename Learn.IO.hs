{- | A haskell script to help learn IO operations
 - File:          Learn.IO.hs
 - Author:        Steven Ward <stevenward94@gmail.com>
 - URL:           https://github.com/StevenWard94/LearningHaskell
 - Last Change:   2016 July 4
 -}

-- NOTE: since only one 'main' function can exist, many are renamed to
-- 'voidmain' (or something similar) to avoid warnings


import Data.Char

-- this is the "active" 'main' function, which can be edited as needed
main = interact respondPalindromes


respondPalindromes =
    unlines . map (\xs -> if isPalindrome xs then "palindrome" else "not a palindrome") . lines
      where isPalindrome xs = xs == reverse xs


-- re-implementing System.IO.withFile function
withFile' :: FilePath -> IOMode -> (Handle -> IO a) -> IO a
withFile' path mode fx = do
    handle <- openFile path mode
    result <- fx handle
    hClose handle
    return result


-- "inactive" 'main' functions
voidmain = do
    putStrLn "What's your first name?"
    firstName <- getLine
    putStrLn "What's your last name?"
    lastName <- getLine
    let bigFirstName = map toUpper firstName
        bigLastName  = map toUpper lastName
    putStrLn $ "hey " ++ bigFirstName ++ " " ++ bigLastName ++ ", how are you?"

voidmain' = do
    putStrLn "Hello, what is your name?"
    name <- getLine
    putStrLn ("Hey " ++ name ++ ", you rock!")

voidmain'' = do
    withFile "something.txt" ReadMode (\handle -> do
        hSetBuffering handle $ BlockBuffering (Just 2048)
        contents <- hGetContents handle
        putStr contents)
