{- | A haskell example-script that takes some input and prints only the lines under 10 characters
 - Script:        ShortLinesOnly.hs
 - Author:        Steven Ward <stevenward94@gmail.com>
 - URL:           https://github.com/StevenWard94/LearningHaskell
 - Last Change:   2016 June 29
 -}


{- main = do
 -     contents <- getContents
 -     putStr (onlyShortLn contents)
 -
 - onlyShortLn :: String -> String
 - onlyShortLn input =
 -     let linesIn = lines input
 -         shortLn = filter (\line -> length line < 10) linesIn
 -     in  unlines shortLn
 --
 -- the above version does not use the 'interact' function, while the
 -- version below does
 --
 -}

main = interact $ unlines . filter ((<10) . length) . lines

