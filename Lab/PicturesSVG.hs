{- | Haskell script to supply dependencies for "Lab 1 Chess" scripts
       Credit for most of this script goes to:
              Haskell: The Craft of Functional Programming, 3e
              Simon Thompson
              (c) Addison-Wesley, 1996-2011
 - Host URL:      https://github.com/StevenWard94/LearningHaskell
 - Last Change:   2016 June 21
 -}


module PicturesSVG where

import System.IO
import Test.QuickCheck
import Control.Monad (liftM, liftM2)

-- Pictures represented by a type of trees - so this is a
-- deep embedding

data Picture
  = Img Image
  | Above Picture Picture
  | Beside Picture Picture
  | Over Picture Picture
  | FlipH Picture
  | FlipV Picture
  | Invert Picture
  deriving (Eq,Show)

-- Coordinates are Integer pairs denoted (x,y)
--
--  o------> x axis
--  |
--  |
--  V
--  y axis


type Point = (Int, Int)

-- The Point of an Image gives its dimension in pixels

data Image = Image Name Point
             deriving (Eq,Show)

data Name  = Name String
             deriving (Eq,Show)

--
-- The functions over Pictures
--

above, beside, over :: Picture -> Picture -> Picture

above  = Above
beside = Beside
over   = Over

-- 'flipH' is 'flip' on the horizontal axis
-- 'flipV' is 'flip' on the vertical axis
-- 'negative' negates each pixel

-- The definitions of 'flipH', 'flipV', & 'negative' push the
-- constructors through the binary operations to the images
-- at the leaves.

-- Original implementation incorrect: it pushed 'flipH' and
-- 'flipV' through all the constructors...
-- Now it distributes appropriately over 'Above', 'Beside' and 'Over'.

flipH, flipV, invert :: Picture -> Picture

flipH (Above pic1 pic2)  = flipH pic2 `Above` flipH pic1
flipH (Beside pic1 pic2) = flipH pic1 `Beside` flipH pic2
flipH (Over pic1 pic2)   = flipH pic1 `Over` flipH pic2
flipH pic                = FlipH pic

flipV (Above pic1 pic2)  = flipV pic1 `Above` flipV pic2
flipV (Beside pic1 pic2) = flipV pic2 `Beside` flipV pic1
flipV (Over pic1 pic2)   = flipV pic1 `Over` flipV pic2
flipV pic                = FlipV pic

invert = Invert
invertColor = Invert
invertColour = invertColor

-- Convert an Image to a Picture

img :: Image -> Picture

img = Img

--
-- Library Functions
--

-- Dimensions of pictures

width, height :: Picture -> Int

width (Img (Image _ (x,_))) = x
width (Above pic1 pic2)     = max (width pic1) (width pic2)
width (Beside pic1 pic2)    = width pic1 + width pic2
width (Over pic1 pic2)      = max (width pic1) (width pic2)
width (FlipH pic)           = width pic
width (FlipV pic)           = width pic
width (Invert pic)          = width pic

height (Img (Image _ (x,y))) = y
height (Above pic1 pic2)     = height pic1 + height pic2
height (Beside pic1 pic2)    = max (height pic1) (height pic2)
height (Over pic1 pic2)      = max (height pic1) (height pic2)
height (FlipH pic)           = height pic
height (FlipV pic)           = height pic
height (Invert pic)          = height pic

--
-- Converting pictures to a list of basic images.
--

-- A 'Filter' represents which of the actions of 'flipH', 'flipV'
-- and 'invert' is to be applied to an image in forming a 'Basic'
-- picture

data Filter = Filter {fH, fV, neg :: Bool}
              deriving (Show)

newFilter = Filter False False False

data Basic = Basic Image Point Filter
             deriving (Show)

-- Flatten a picture into a list of 'Basic' pictures.
-- The 'Point' argument gives the origin for the conversion of
-- the argument.

flatten :: Point -> Picture -> [Basic]

flatten (x,y) (Img image)        = [Basic image (x,y) newFilter]
flatten (x,y) (Above pic1 pic2)  = flatten (x,y) pic1 ++ flatten (x, y + height pic1) pic2
flatten (x,y) (Beside pic1 pic2) = flatten (x,y) pic1 ++ flatten (x + width pic1 , y) pic2
flatten (x,y) (Over pic1 pic2)   = flatten (x,y) pic2 ++ flatten (x,y) pic1
flatten (x,y) (FlipH pic)        = map flipFH $ flatten (x,y) pic
flatten (x,y) (FlipV pic)        = map flipFV $ flatten (x,y) pic
flatten (x,y) (Invert pic)       = map flipNeg $ flatten (x,y) pic
