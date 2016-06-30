{- | A simple script that outputs the contents of 'girlfriend.txt', which contains some
      lyrics from Avril Lavigne's song, "Girlfriend"
 - Script:        girlfriend.hs
 - Author:        Steven Ward <stevenward94@gmail.com>
 - URL:           https://github.com/StevenWard94/LearningHaskell
 - Last Change:   2016 June 29
 -}

import System.IO

main = do
    handle <- openFile "girlfriend.txt" ReadMode
    contents <- hGetContents handle
    putStr contents
    hClose handle

-------------------------------------------------------------------------------
-- NOTE(S) TO SELF
   ---------------
--    openFile :: FilePath -> IOMode -> IO Handle
--
--  FilePath is just a type synonym:
--    type FilePath = String
--
--  IOMode is a type defined like so:
--    data IOMode = ReadMode | WriteMode | AppendMode | ReadWriteMode
-------------------------------------------------------------------------------
