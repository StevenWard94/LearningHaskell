{- | A haskell script to get which colors the user associates with the numbers 1 through 4
 - Script:        Colors.hs
 - Author:        Steven Ward <stevenward94@gmail.com>
 - URL:           https://github.com/StevenWard94/LearningHaskell
 - Last Change:   2016 June 28
 -}

import Control.Monad

main = do
    colors <- forM [1,2,3,4] (\a -> do
        putStrLn $ "Which color do you associate with the number " ++ show a ++ "?"
        color <- getLine
        return color)
    putStrLn "The colors you associate with 1, 2, 3 and 4 are: "
    mapM putStrLn colors

