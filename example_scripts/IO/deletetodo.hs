{- | Slightly longer example-script that removes an entry from 'todo.txt'
 - Script:        deletetodo.hs
 - Author:        Steven Ward <stevenward94@gmail.com>
 - URL:           https://github.com/StevenWard94/LearningHaskell
 - Last Change:   2016 July 4
 -}

import System.IO
import System.Directory
import Data.List


main = do
    handle <- openFile "todo.txt" ReadMode
    (tempName, tempHandle) <- openTempFile "." "temp"
    contents <- hGetContents handle
    let todoList = lines contents
        tasks    = zipWith (\n line -> show n ++ " - " ++ line) [0..] todoList
    putStrLn "These are your TO-DO items:"
    putStr $ unlines tasks
    putStrLn "Which task would you like to remove?"
    numberStr <- getLine
    let number = read numberStr
        newToDo = delete (todoList !! number) todoList
    hPutStr tempHandle $ unlines newTodoItems
    hClose handle
    hClose tempHandle
    removeFile "todo.txt"
    renameFile tempName "todo.txt"
