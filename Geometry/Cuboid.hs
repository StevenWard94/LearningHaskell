{- | Learning to create Haskell modules in hierarchical structure
 - Module:        Geometry.Cuboid
 - Author:        Steven Ward <stevenward94@gmail.com>
 - URL:           https://github.com/StevenWard94/LearningHaskell
 - Last Change:   2016 June 11
 -}

module Geometry.Cuboid
( volume
, area
) where

volume :: Float -> Float -> Float -> Float
volume l w h = rectangleArea l w * h

area :: Float -> Float -> Float -> Float
area l w h = (rectangleArea l w + rectangleArea l h + rectangleArea h w) * 2

rectangleArea :: Float -> Float -> Float
rectangleArea l w = l * w
