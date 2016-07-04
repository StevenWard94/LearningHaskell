{- | A simple example-script that demonstrates 'appendFile' by adding items to a TODO list
 - Script:        appendtodo.hs
 - Author:        Steven Ward <stevenward94@gmail.com>
 - URL:           https://github.com/StevenWard94/LearningHaskell
 - Last Change:   2016 July 4
 -}

import System.IO

main = do
    itemToDo <- getLine
    appendFile "todo.txt" (itemToDo ++ "\n")
