-- |
-- Module:        Functional.Problems
-- Author:        Steven Ward <stevenward94@gmail.com>
-- URL:           https://github.com/StevenWard94/LearningHaskell
-- Last Change:   2016 July 28
--

module Functional.Problems
    ( solveRPN, solveRPNf
    ) where

import Data.List

-- | main function is related to the RoadSystem problem
main = do
    contents <- getContents
    let threes = groupsOf 3 (map read $ lines contents)
        rdSystem = map (\[a,b,c] -> Intersection a b c) threes
        path = optimalPath rdSystem
        pathStr = concatMap (show . fst) path
        pathTime = sum $ map snd path
    putStrLn $ "The fastest route is: " ++ pathStr
    putStrLn $ "The total time is: " ++ show pathTime

-- | a function that takes an expression in reverse polish notation,
-- evaluates it, and returns the result
solveRPN :: (Num a, Read a) => String -> a
solveRPN = head . foldl f [] . words
    where f (x:y:ys) "*" = (x * y):ys
          f (x:y:ys) "+" = (x + y):ys
          f (x:y:ys) "-" = (x - y):ys
          f xs expr      = read expr:xs

-- | overload of solveRPN to return floating point numbers
solveRPNf :: String -> Float
solveRPNf = head . foldl f [] . words
    where f (x:y:ys) "*" = (x * y):ys
          f (x:y:ys) "+" = (x + y):ys
          f (x:y:ys) "-" = (x - y):ys
          f (x:y:ys) "/" = (x / y):ys
          f (x:y:ys) "^" = (x ** y):ys
          f (x:xs) "ln"  = log x:xs
          f xs "sum"     = [sum xs]
          f xs expr      = read expr:xs

-- | solution algorithm for the problem of determining the shortest driving
-- time between two points from the travel time of each section of a number
-- of roads (a "section" of road being the segment between two "crossroads")

-- | definitions for a typeclass and type synonym
data Intersection = Intersection
    { getA :: Int      -- ^ time for road segment 'A'
    , getB :: Int      -- ^ time for road segment 'B'
    , getC :: Int      -- ^ time for road segment 'C'
    } deriving (Show)

type RoadSystem = [Intersection]

-- | Enum type used to name road segments at Intersections
--     note: each "RoadSystem" is formed by 2 parallel roads (A and B)
--     connected by a number of crossroads (all C) perpendicular to A,B
data Segment = A | B | C deriving (Show)

-- | Type synonym for a list of tuples containing a 'Segment' and number,
-- which will represent any route from the start of a trip to an arbitrary point
type Path = [ (Segment, Int) ]    -- the tuple, (Segment,Int), is a "choice"

exRoadSystem :: RoadSystem
exRoadSystem = [ Intersection 50 10 30, Intersection 5 90 20, Intersection 40 2 25, Intersection 10 8 0 ]

-- | helper function that takes a pair of paths and an intersection, and
-- returns a new pair of paths, which represent the "new" optimal path
tripLeg :: (Path, Path) -> Intersection -> (Path, Path)
tripLeg (pathA, pathB) (Intersection a b c) =
    let timeA = sum $ map snd pathA
        timeB = sum $ map snd pathB
        fwdTimeToA = timeA + a
        crossTimeToA = timeB + b + c
        fwdTimeToB = timeB + b
        crossTimeToB = timeA + a + c
        newPathA = if fwdTimeToA <= crossTimeToA
                      then (A,a):pathA
                      else (C,c):(B,b):pathB
        newPathB = if fwdTimeToB <= crossTimeToB
                      then (B,b):pathB
                      else (C,c):(A,a):pathA
    in (newPathA, newPathB)

-- | final solution algorithm to solve the problem of finding the shortest
-- route through a system or roads
optimalPath :: RoadSystem -> Path
optimalPath rdSystem =
    let (bestAPath, bestBPath) = foldl tripLeg ([], []) rdSystem
    in  if sum (map snd bestAPath) <= sum (map snd bestBPath)
           then reverse bestAPath
           else reverse bestBPath

groupsOf :: Int -> [a] -> [[a]]
groupsOf 0 _ = undefined
groupsOf _ [] = []
groupsOf n xs = take n xs : groupsOf n (drop n xs)
