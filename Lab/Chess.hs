{- | Haskell script for manipulating chess images from "Lab 1" exercises
       found at: https://www.inf.ed.ac.uk/teaching/courses/inf1/fp/tutorials/Labweek.pdf
 - Module:        Lab.Chess
 - Author:        Steven Ward <stevenward94@gmail.com>
 - URL:           https://github.com/StevenWard94/LearningHaskell
 - Last Change:   2016 June 21
 -}

module Lab.Chess
  ( emptyRow
  , otherEmptyRow
  , middleBoard
  , whiteRow
  , blackRow
--, populatedBoard
--, twoBeside
--, twoAbove
--, fourPictures
  ) where

import PicturesSVG
import Test.QuickCheck

emptyRow :: Picture
emptyRow = repeatH 4 (whiteSquare `beside` blackSquare)

otherEmptyRow :: Picture
otherEmptyRow = flipV emptyRow

middleBoard :: Picture
middleBoard = repeatV 2 (emptyRow `above` otherEmptyRow)

whiteRow :: Picture
whiteRow =
          let
              pieces = rook `beside` (knight `beside` (bishop `beside` (queen `beside` (king `beside` (bishop `beside` (knight `beside` rook))))))
           in
              pieces `over` otherEmptyRow

blackRow :: Picture
blackRow = invert whiteRow
