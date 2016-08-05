-- |
-- Module:        knight_quest.hs
-- Author:        Steven Ward <stevenward94@gmail.com>
-- URL:           https://github.com/StevenWard94/LearningHaskell.d
-- Last Change:   2016 Aug 04
--
module KnightQuest where    -- dummy module declaration

-- | A problem that really lends itself to being solved with
-- non-determinism [source: http://learnyouahaskell.com/a-fistful-of-monads]
--
-- Say you have a chess board with only a single knight on it. We want to
-- find out if the knight can reach a certain position in 3 moves. A pair
-- of numbers will represent the knight's position on the chess board,
-- where the first number is the column and the second number is the row.

import qualified Control.Monad as CM
------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------

-- type synonym for the knight's current position on the board
type KnightPos = (Int,Int)

-- instead of determining the single "best" move, we use non-determinism to
-- "pick" ALL moves it can possibly make next!
-- This function take's the knight's position and returns all possible choices
-- for it's next move:
moveKnight :: KnightPos -> [KnightPos]
moveKnight (c,r) = do
    (c',r') <- [(c+2,r-1),(c+2,r+1),(c-2,r-1),(c-2,r+1)
              ,(c+1,r-2),(c+1,r+2),(c-1,r-2),(c-1,r+2)]
    CM.guard (c' `elem` [1..8] && r' `elem` [1..8])
    return (c',r')

-- for demonstration purposes, here is the same function written without
-- the use of lists as a monad:
mvKnightL :: KnightPos -> [KnightPos]
mvKnightL (c,r) = filter onBoard
    [(c+2,r-1),(c+2,r+1),(c-2,r-1),(c-2,r+1)
    ,(c+1,r-2),(c+1,r+2),(c-1,r-2),(c-1,r+2)
    ]
    where onBoard (c,r) = c `elem` [1..8] && r `elem` [1..8]

-- now that we have a non-deterministic "next" position, we just use the
-- 'bind' function (>>=) to feed it to 'moveKnight'.
-- Here's a function that takes a position and returns all of the positions
-- that can be reached from that initial position in three moves
in3 :: KnightPos -> [KnightPos]
in3 start = do
    first <- moveKnight start
    second <- moveKnight first
    moveKnight second

-- again, here is that function w/out 'do' notation
in3dont :: KnightPos -> [KnightPos]
in3dont start = return start >>= moveKnight >>= moveKnight >>= moveKnight

-- Now, here's a function that takes two positions and determines if the
-- knight can get from one to the other in EXACTLY 3 steps
canReachIn3 :: KnightPos -> KnightPos -> Bool
canReachIn3 start end = end `elem` in3 start
