{- | Haskell script for manipulating chess images from "Lab 1" exercises
       found at: https://www.inf.ed.ac.uk/teaching/courses/inf1/fp/tutorials/Labweek.pdf
 - Module:        Lab.Chess
 - Author:        Steven Ward <stevenward94@gmail.com>
 - URL:           https://github.com/StevenWard94/LearningHaskell
 - Last Change:   2016 June 21
 -}

module Lab.Chess
  ( pic1
  , pic2
  , emptyRow
  , otherEmptyRow
  , middleBoard
  , whiteRow
  , blackRow
  , populatedBoard
  , twoBeside
  , twoAbove
  , fourPictures
  ) where

import PicturesSVG
import Test.QuickCheck
