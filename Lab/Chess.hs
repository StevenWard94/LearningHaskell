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
  , whitePawns
  , blackPawns
  , populatedBoard
  , twoBeside
  , twoAbove
  , fourPictures
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
blackRow =
          let
              pieces = rook `beside` (knight `beside` (bishop `beside` (queen `beside` (king `beside` (bishop `beside` (knight `beside` rook))))))
           in
              invert pieces `over` emptyRow

whitePawns :: Picture
whitePawns = repeatH 8 pawn `over` emptyRow

blackPawns :: Picture
blackPawns = repeatH 8 (invert pawn) `over` otherEmptyRow

populatedBoard :: Picture
populatedBoard = blackRow `above` blackPawns `above` middleBoard `above` whitePawns `above` whiteRow

twoBeside :: Picture -> Picture
twoBeside pic = pic `beside` (invert pic)

twoAbove :: Picture -> Picture
twoAbove pic = pic `above` (invert pic)

fourPictures :: Picture -> Picture
fourPictures pic = twoAbove (twoBeside pic)
