-- |
-- Module:        functor_stuff.hs
-- Author:        Steven Ward <stevenward94@gmail.com>
-- URL:           https://github.com/StevenWard94/LearningHaskell
-- Last Change:   2016 July 28
--

import Data.Char
import Data.List

main = do line <- fmap (intersperse '-' . reverse . map toUpper) getLine
          putStrLn line


main' = do line <- fmap reverse getLine
          putStrLn $ "You said " ++ line ++ " backwards!
