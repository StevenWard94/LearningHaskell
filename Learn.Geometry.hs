{- | Practicing creating my own module w/ geometry functions
 - Module:          Learn.Geometry
 - Author:          Steven Ward <stevenward94@gmail.com>
 - URL:             https://github.com/StevenWard94/LearningHaskell
 - Last Change:     2016 June 11
 -}

module Learn.Geometry
( sphereVolume
, sphereArea
, cubeVolume
, cubeArea
, cuboidArea
, cuboidVolume
) where

sphereVolume :: Float -> Float
sphereVolume r = (4.0 / 3.0) * pi * (r ^ 3)

sphereArea :: Float -> Float
sphereArea r = 4 * pi * (r ^ 2)

cubeVolume :: Float -> Float
cubeVolume side = cuboidVolume side side side

cubeArea :: Float -> Float
cubeArea side = cuboidArea side side side

cuboidVolume :: Float -> Float -> Float -> Float
cuboidVolume l w h = rectangleArea l w * h

cuboidArea :: Float -> Float -> Float -> Float
cuboidArea l w h = (rectangleArea l w + rectangleArea l h + rectangleArea h w) * 2

-- rectangleArea is just a helper function and IS NOT EXPORTED as part of
-- the module
rectangleArea :: Float -> Float -> Float
rectangleArea l w = l * w
